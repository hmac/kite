-- The constraint generator

module Constraint.Generate where

import           Data.Set                       ( (\\) )
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import           Control.Monad                  ( when )

import           Constraint.Generate.M
import           Constraint
import           Constraint.Expr
import           Constraint.Generate.Pattern
import           Constraint.Primitive           ( fcallInfo )
import           Data.String                    ( IsString(fromString) )

import           Util

generate :: TypeEnv -> Exp -> GenerateM Error (ExpT, Type, Constraints)
-- VARCON
generate env (Var name) = case Map.lookup name env of
  Just (Forall tvars t) -> do
    subst <- Map.fromList <$> mapM (\tv -> (tv, ) . TVar <$> fresh) tvars
    let t' = sub subst t
    pure (VarT name t', t', mempty)
  Nothing -> throwError (UnknownVariable name)
-- Data constructors are treated identically to variables
generate env (Con n) = do
  (_, t, c) <- generate env (Var n)
  pure (ConT n, t, c)
-- APP
generate env (App e1 e2) = do
  (e1', t1, c1) <- generate env e1
  (e2', t2, c2) <- generate env e2
  a             <- TVar <$> fresh
  let funcConstraint = t1 :~: (t2 `fn` a)
  pure (AppT e1' e2', a, funcConstraint : (c1 <> c2))
-- ABS
generate env (Abs xs e) = do
  binds <- mapM (\x -> (x, ) . TVar <$> fresh) xs
  let env' = foldl (\env_ (x, t) -> Map.insert x (Forall [] t) env_) env binds
  (e', t, c) <- generate env' e
  let ty = foldr (\(_, a) b -> a `fn` b) t binds
  pure (AbsT binds e', ty, c)
-- LET: let with no annotation
generate env (Let binds body) = do
  -- extend the environment simultaneously with all variables
  binds' <- mapM (\(x, e) -> (x, , e) . TVar <$> fresh) binds
  let env' = foldl (\e (x, t, _) -> Map.insert x (Forall [] t) e) env binds'
  -- infer each bound expression with the extended environment
  (xs, _ts, es, cs) <-
    unzip4
      <$> mapM
            (\(x, t, e) -> do
              (e', t', c) <- generate env' e
              let c' = t :~: t'
              pure (x, t, e', c' : c)
            )
            binds'
  -- infer the body with the extended environment
  (body', bodyT, bodyC) <- generate env' body
  pure (LetT (zip xs es) body' bodyT, bodyT, bodyC <> mconcat cs)
-- LETA: let with a monomorphic annotation
generate env (LetA x (Forall [] t1) e1 e2) = do
  (e1', t , c1) <- generate env e1
  (e2', t2, c2) <- generate (Map.insert x (Forall [] t1) env) e2
  pure (LetAT x (Forall [] t1) e1' e2' t2, t2, t :~: t1 : c1 <> c2)
-- GLETA: let with a polymorphic annotation
-- TODO: if we remove this type of let, we can get rid of E constraints
generate env (LetA x s1@(Forall _ t1) e1 e2) = do
  (e1', t, c) <- generate env e1
  let betas = Set.toList $ (fuv t <> fuv c) \\ fuv env
  let c1    = E betas mempty (t :~: t1 : c)
  (e2', t2, c2) <- generate (Map.insert x s1 env) e2
  pure (LetAT x s1 e1' e2' t2, t2, c1 : c2)
-- CASE: case expression
generate env (Case e alts) = generateCase env e alts
-- Expression hole
generate _   (Hole name  ) = do
  a <- TVar <$> fresh
  pure (HoleT name a, a, mempty)
-- Unit literal
generate _env UnitLit          = pure (UnitLitT TUnit, TUnit, mempty)
-- Tuple literal
generate env  (TupleLit elems) = do
  (elems', elemTypes, constraints) <- unzip3 <$> mapM (generate env) elems
  let t = mkTupleType elemTypes
  pure (TupleLitT elems' t, t, mconcat constraints)
-- List literal
generate env (ListLit elems) = do
  beta <- TVar <$> fresh
  let t = list beta
  (elems', elemTypes, constraints) <- unzip3 <$> mapM (generate env) elems
  let sameTypeConstraint = map (beta :~:) elemTypes
  pure (ListLitT elems' t, t, sameTypeConstraint <> mconcat constraints)
-- Int literal
generate _env (IntLit  i     ) = pure (IntLitT i TInt, TInt, mempty)
-- Bool literal
generate _env (BoolLit b     ) = pure (BoolLitT b TBool, TBool, mempty)
-- Char literal
generate _env (CharLit c     ) = pure (CharLitT c TChar, TChar, mempty)
-- String literal
generate env  (StringLit p cs) = do
  (cs', constraints) <- unzip <$> forM
    cs
    (\(e, s) -> do
      (e', cty, c) <- generate env e
      -- Each interpolated expression must have type String
      let strConstraint = cty :~: TString
      pure ((e', s), strConstraint : c)
    )
  pure (StringLitT p cs' TString, TString, mconcat constraints)
-- Record
generate env (Record fields) = do
  let fieldLabels = map fst fields
      fieldExprs  = map snd fields
  (fieldExprs', fieldTypes, constraints) <-
    unzip3 <$> mapM (generate env) fieldExprs
  let ty      = TRecord (zip fieldLabels fieldTypes)
  let record' = RecordT (zip fieldLabels fieldExprs') ty
  pure (record', ty, mconcat constraints)
generate env (Project record label) = do
  (record', recordType, recordConstraints) <- generate env record
  -- Generate a tyvar beta for the projection
  beta <- TVar <$> fresh
  -- Constraint recordType to have a field with label l and type beta
  let fieldConstraint = HasField recordType label beta
  pure
    ( ProjectT record' label recordType
    , beta
    , fieldConstraint : recordConstraints
    )
-- FFI calls
generate env (FCall fcallName fcallArgs) = case lookup fcallName fcallInfo of
  Just (vars, argTypes, resTy) -> do
    -- check number of args
    when (length argTypes /= length fcallArgs)
      $ throwError
      $ WrongNumberOfArgsToForeignCall fcallName
                                       (length argTypes)
                                       (length fcallArgs)
    let argsAndTypes = zip fcallArgs argTypes

    -- generate uvars for rvars
    uvars <- forM vars $ const $ TVar <$> fresh
    let varSubst = Map.fromList $ zip vars uvars

    -- check arg types (substituting vars)
    (args, _, cs) <- fmap unzip3 $ forM argsAndTypes $ \(arg, ty) -> do
      let expectedType = sub varSubst ty
      (arg', argTy, argC) <- generate env arg
      pure (arg', argTy, argTy :~: expectedType : argC)

    -- return result type (substituting vars)
    let resTy' = sub varSubst resTy
    pure (FCallT fcallName args resTy', resTy', mconcat cs)
  Nothing -> throwError $ UnknownVariable (fromString fcallName)

-- Case expressions
-------------------
-- We use the simplified version from Fig 6 because Lam doesn't have GADTs. If
-- it turns out that typeclasses need the more complex version, this will need
-- to be changed.
generateCase
  :: TypeEnv
  -> Exp
  -> [(Pattern, Exp)]
  -> GenerateM Error (ExpT, Type, Constraints)

-- Lam doesn't support empty case expressions.
generateCase _env _e        []   = throwError EmptyCase

generateCase env  scrutinee alts = do
  -- infer the scrutinee
  (scrutineeT, scrutTy, scrutC) <- generate env scrutinee
  -- infer each case alternative
  (es, patTys, expTys, cs)      <-
    unzip4 <$> mapM (\(p, e) -> generateEquation env (p, e)) alts
  -- all top level patterns must have the same type, equal to the scrutinee type
  let allPatsEq = generateAllEqualConstraint scrutTy patTys
  -- all corresponding branches must have the same type
  (_beta2, allExpsEq) <- do
    beta <- TVar <$> fresh
    pure (beta, generateAllEqualConstraint beta expTys)
  -- TODO: is it ok for all of these to be touchables?
  let allConstraints = scrutC <> allPatsEq <> allExpsEq <> mconcat cs
  let caseTy         = head expTys
  let altsT          = zipWith (\e' (p, _) -> AltT p e') es alts
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
  :: TypeEnv
  -> (Pattern, Exp)
  -> GenerateM Error (ExpT, Type, Type, Constraints)
generateEquation env (pat, expr) = do
  (patTy, patC , env') <- fresh >>= \t -> generatePattern env (TVar t) pat
  (e    , expTy, expC) <- generate env' expr
  pure (e, patTy, expTy, patC <> expC)

-- Generates a constraint requiring all the given types to be equal to each
-- other
generateAllEqualConstraint :: Type -> [Type] -> Constraints
generateAllEqualConstraint t ts =
  fst $ foldl (\(c, t') u -> ((t' :~: u) : c, t)) (mempty, t) ts
