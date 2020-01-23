{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- The constraint generator

module Constraint.Generate where

import           Util
import           Data.Set                       ( (\\) )
import qualified Data.Set                      as Set

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import           Data.Name
import           Constraint.Generate.M
import           Constraint
import           Constraint.Expr

type Env = Map RawName Scheme

instance Sub Env where
  sub s = fmap (sub s)

generate :: Env -> Exp -> GenerateM (ExpT, Type, CConstraint)
-- VARCON
generate env (Var name) = case Map.lookup name env of
  Just (Forall tvars c t) -> do
    subst <- mapM (\tv -> (tv, ) . TVar <$> fresh) tvars
    let t' = sub subst t
    let q' = sub subst c
    pure (VarT name t', t', Simple q')
  Nothing -> do
    a <- TVar <$> fresh
    pure (VarT name a, a, mempty)
-- Data constructors are treated identically to variables
generate env (Con (C n)) = do
  (_, t, c) <- generate env (Var n)
  pure (ConT (C n), t, c)
-- APP
generate env (App e1 e2) = do
  (e1, t1, c1) <- generate env e1
  (e2, t2, c2) <- generate env e2
  a            <- TVar <$> fresh
  let funcConstraint = Simple $ t1 :~: (t2 `fn` a)
  pure (AppT e1 e2, a, c1 <> (c2 <> funcConstraint))
-- ABS
generate env (Abs x e) = do
  a         <- TVar <$> fresh
  (e, t, c) <- generate (Map.insert x (Forall [] CNil a) env) e
  pure (AbsT x a e, a `fn` t, c)
-- LET: let with no annotation
generate env (Let x e1 e2) = do
  (e1, t1, c1) <- generate env e1
  (e2, t2, c2) <- generate (Map.insert x (Forall [] CNil t1) env) e2
  pure (LetT x e1 e2 t2, t2, c1 <> c2)
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

-- CASE
-- We use the simplified version from Fig 6 because Lam doesn't have GADTs. If
-- it turns out that typeclasses need the more complex version, this will need
-- to be changed.

-- Lam doesn't support empty cases. If there are no cases, just return a new
-- unification variable, which will cause a type error
generate env (Case e []) = do
  (e, _, _) <- generate env e
  a         <- TVar <$> fresh
  pure (CaseT e [] a, a, mempty)
generate env (Case e alts) = do
  (e, t, c) <- generate env e
  beta      <- TVar <$> fresh
  -- if any of the alts contain a simple constructor pattern, use this to find
  -- the relevant type constructor
  case findConTypeInAlts env alts of
    -- If one of the alts is a constructor pattern, generate uvars for the type
    -- variables
    Just (Forall tvars _ tk) -> do
      let (TCon tyname _) = last (unfoldFnType tk)
      ys <- mapM (const fresh) tvars
      let c' = Simple (TCon tyname (map TVar ys) :~: t) <> c
      (alts, cis) <- unzip <$> mapM (genAlt env beta t ys) alts
      let c'' = c' <> mconcat cis
      pure (CaseT e alts beta, beta, c'')
    -- otherwise, the patterns must all be VarPats, so we just need a single
    -- uvar for the scrutinee type
    Nothing -> do
      (alts, cis) <- unzip <$> mapM (genAlt env beta t []) alts
      let c' = c <> mconcat cis
      pure (CaseT e alts beta, beta, c')
generate _ (Hole name) = do
  a <- TVar <$> fresh
  pure (HoleT name a, a, mempty)
generate env (TupleLit elems) = do
  (elems', elemTypes, constraints) <- unzip3 <$> mapM (generate env) elems
  let t = mkTupleType elemTypes
  pure (TupleLitT elems' t, t, mconcat constraints)
generate env (ListLit elems) = do
  beta <- TVar <$> fresh
  let t = list beta
  (elems', elemTypes, constraints) <- unzip3 <$> mapM (generate env) elems
  let sameTypeConstraint = mconcat $ map (beta :~:) elemTypes
  pure (ListLitT elems' t, t, mconcat constraints <> Simple sameTypeConstraint)
generate env (IntLit i      ) = pure (IntLitT i TInt, TInt, mempty)
generate env (StringLit p cs) = do
  -- TODO: each expression's type should be in the Show typeclass
  (cs', constraints) <- unzip <$> forM
    cs
    (\(e, s) -> do
      (e', _, c) <- generate env e
      pure ((e', s), c)
    )
  pure (StringLitT p cs' TString, TString, mconcat constraints)

findConTypeInAlts :: Env -> [Alt] -> Maybe Scheme
findConTypeInAlts _ [] = Nothing
findConTypeInAlts env (Alt (SConPat (C name) _) _ : alts) =
  case Map.lookup name env of
    Just t  -> Just t
    Nothing -> findConTypeInAlts env alts
findConTypeInAlts env (_ : alts) = findConTypeInAlts env alts

-- beta: a uvar representing the type of the whole case expression
-- ys:   uvars for each argument to the type constructor of the scrutinee
genAlt :: Env -> Type -> Type -> [Var] -> Alt -> GenerateM (AltT, CConstraint)
genAlt env beta _ ys (Alt (SConPat (C k) xi) e) = case Map.lookup k env of
  Nothing -> do
    a         <- TVar <$> fresh
    (e, _, _) <- generate env e
    pure (AltT (SConPat (C k) xi) e, Simple (a :~: TCon k []))
  Just (Forall as _ kt) -> do
    -- construct substitution
    let subst = zip as (map TVar ys)
    let us    = init (unfoldFnType kt)
    let us'   = map (Forall [] CNil . sub subst) us
    let env'  = Map.fromList (zip xi us') <> env
    -- check ei under assumption that all xi have type [ys/as]t
    (e, ti, ci) <- generate env' e
    let c' = ci <> Simple (ti :~: beta)
    pure (AltT (SConPat (C k) xi) e, c')

genAlt env beta scrutTy _ (Alt (SVarPat x) e) = do
  u <- TVar <$> fresh
  -- check e under the assumption that x has type u
  let env' = Map.insert x (Forall [] CNil u) env
  (e, t, c) <- generate env' e
  -- require u ~ the type of the scrutinee
  let c' = Simple (u :~: scrutTy)
  pure (AltT (SVarPat x) e, c <> c' <> Simple (t :~: beta))

genAlt env beta _ _ (Alt SWildPat e) = do
  -- The wildcard can't be used inside e, so we don't need to extend the
  -- environment.
  (e, t, c) <- generate env e
  pure (AltT SWildPat e, c <> Simple (t :~: beta))

-- Converts a -> b -> c into [a, b, c]
unfoldFnType :: Type -> [Type]
unfoldFnType (TCon "->" [x, y]) = x : unfoldFnType y
unfoldFnType t                  = [t]

mkTupleType :: [Type] -> Type
mkTupleType args = TCon name args
 where
  name = case length args of
    0 -> "Unit"
    2 -> "Tuple2"
    3 -> "Tuple3"
    4 -> "Tuple4"
    5 -> "Tuple5"
    6 -> "Tuple6"
    7 -> "Tuple7"
    n -> error $ "Unsupported tuple length: " <> show n

-- Generates a constraint requiring all the given types to be equal to each
-- other
generateAllEqualConstraint :: Type -> [Type] -> Constraint
generateAllEqualConstraint t ts =
  fst $ foldl (\(c, t') u -> (c <> t' :~: u, t)) (mempty, t) ts
