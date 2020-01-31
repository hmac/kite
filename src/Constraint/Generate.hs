-- The constraint generator

module Constraint.Generate where

import           Util
import           Data.Set                       ( (\\) )
import qualified Data.Set                      as Set

import qualified Data.Map.Strict               as Map

import           Constraint.Generate.M
import           Constraint
import           Constraint.Expr
import           Constraint.Generate.Pattern

generate :: Env -> Exp -> GenerateM (ExpT, Type, CConstraint)
-- VARCON
generate env (Var name) = case Map.lookup name env of
  Just (Forall tvars c t) -> do
    subst <- mapM (\tv -> (tv, ) . TVar <$> fresh) tvars
    let t' = sub subst t
    let q' = sub subst c
    pure (VarT name t', t', Simple q')
  Nothing -> throwError (UnknownVariable name)
-- Data constructors are treated identically to variables
generate env (Con n) = do
  (_, t, c) <- generate env (Var n)
  pure (ConT n, t, c)
-- APP
generate env (App e1 e2) = do
  (e1, t1, c1) <- generate env e1
  (e2, t2, c2) <- generate env e2
  a            <- TVar <$> fresh
  let funcConstraint = Simple $ t1 :~: (t2 `fn` a)
  pure (AppT e1 e2, a, c1 <> (c2 <> funcConstraint))
-- ABS
generate env (Abs xs e) = do
  binds <- mapM (\x -> (x, ) . TVar <$> fresh) xs
  let env' = foldl (\e (x, t) -> Map.insert x (Forall [] CNil t) e) env binds
  (e, t, c) <- generate env' e
  let ty = foldr (\(_, a) b -> a `fn` b) t binds
  pure (AbsT binds e, ty, c)
-- LET: let with no annotation
generate env (Let binds body) = do
  -- extend the environment simultaneously with all variables
  binds <- mapM (\(x, e) -> (x, , e) . TVar <$> fresh) binds
  let env' =
        foldl (\e (x, t, _) -> Map.insert x (Forall [] CNil t) e) env binds
  -- infer each bound expression with the extended environment
  (xs, ts, es, cs) <-
    unzip4
      <$> mapM
            (\(x, t, e) -> do
              (e', t', c) <- generate env' e
              let c' = Simple (t :~: t')
              pure (x, t, e', c <> c')
            )
            binds
  -- infer the body with the extended environment
  (body, bodyT, bodyC) <- generate env' body
  pure (LetT (zip xs es) body bodyT, bodyT, bodyC <> mconcat cs)
-- LETA: let with a monomorphic annotation
generate env (LetA x (Forall [] CNil t1) e1 e2) = do
  (e1, t , c1) <- generate env e1
  (e2, t2, c2) <- generate (Map.insert x (Forall [] CNil t1) env) e2
  pure (LetAT x (Forall [] CNil t1) e1 e2 t2, t2, c1 <> c2 <> Simple (t :~: t1))
-- GLETA: let with a polymorphic annotation
generate env (LetA x s1@(Forall _ q1 t1) e1 e2) = do
  (e1, t, c) <- generate env e1
  let betas = Set.toList $ (fuv t <> fuv c) \\ fuv env
  let c1    = E betas q1 (c :^^: Simple (t :~: t1))
  (e2, t2, c2) <- generate (Map.insert x s1 env) e2
  pure (LetAT x s1 e1 e2 t2, t2, c1 <> c2)
-- CASE: case expression
generate env (Case e alts) = generateCase env e alts
-- Expression hole
generate _   (Hole name  ) = do
  a <- TVar <$> fresh
  pure (HoleT name a, a, mempty)
-- Tuple literal
generate env (TupleLit elems) = do
  (elems', elemTypes, constraints) <- unzip3 <$> mapM (generate env) elems
  let t = mkTupleType elemTypes
  pure (TupleLitT elems' t, t, mconcat constraints)
-- List literal
generate env (ListLit elems) = do
  beta <- TVar <$> fresh
  let t = list beta
  (elems', elemTypes, constraints) <- unzip3 <$> mapM (generate env) elems
  let sameTypeConstraint = mconcat $ map (beta :~:) elemTypes
  pure (ListLitT elems' t, t, mconcat constraints <> Simple sameTypeConstraint)
-- Int literal
generate env (IntLit i      ) = pure (IntLitT i TInt, TInt, mempty)
-- String literal
generate env (StringLit p cs) = do
  -- TODO: each expression's type should be in the Show typeclass
  (cs', constraints) <- unzip <$> forM
    cs
    (\(e, s) -> do
      (e', _, c) <- generate env e
      pure ((e', s), c)
    )
  pure (StringLitT p cs' TString, TString, mconcat constraints)

-- Case expressions
-------------------
-- We use the simplified version from Fig 6 because Lam doesn't have GADTs. If
-- it turns out that typeclasses need the more complex version, this will need
-- to be changed.
generateCase :: Env -> Exp -> [Alt] -> GenerateM (ExpT, Type, CConstraint)

-- Lam doesn't support empty case expressions.
generateCase _env _e        []   = throwError EmptyCase

generateCase env  scrutinee alts = do
  -- infer the scrutinee
  (scrutineeT, scrutTy, scrutC) <- generate env scrutinee
  -- infer each case alternative
  (es, patTys, expTys, cs)      <-
    unzip4 <$> mapM (\(Alt p e) -> generateEquation env (p, e)) alts
  -- all top level patterns must have the same type, equal to the scrutinee type
  let allPatsEq = generateAllEqualConstraint scrutTy patTys
  -- all corresponding branches must have the same type
  (beta2, allExpsEq) <- do
    beta <- TVar <$> fresh
    pure (beta, generateAllEqualConstraint beta expTys)
  -- TODO: is it ok for all of these to be touchables?
  let allConstraints = scrutC <> mconcat cs <> Simple (allPatsEq <> allExpsEq)
  let caseTy         = head expTys
  let altsT = zipWith (\e' (Alt p _) -> AltT p e') es alts
  let caseT          = CaseT scrutineeT altsT caseTy
  pure (caseT, caseTy, allConstraints)

-- Generates constraints for a single branch of a multi-equation case expression
-- e.g. case l of
--        []       -> Nothing
--        (x :: _) -> Just x
-- generateEquation ([], Nothing)
-- generateEquation ((x :: _), Just x)
-- TODO: consider merging with Constraint.Generate.Bind.generateMultiEquation
generateEquation
  :: Env -> (Pattern, Exp) -> GenerateM (ExpT, Type, Type, CConstraint)
generateEquation env (pat, expr) = do
  (patTy, patC , env') <- fresh >>= \t -> generatePattern env (TVar t) pat
  (e    , expTy, expC) <- generate env' expr
  pure (e, patTy, expTy, patC <> expC)

-- Generates a constraint requiring all the given types to be equal to each
-- other
generateAllEqualConstraint :: Type -> [Type] -> Constraint
generateAllEqualConstraint t ts =
  fst $ foldl (\(c, t') u -> (c <> t' :~: u, t)) (mempty, t) ts
