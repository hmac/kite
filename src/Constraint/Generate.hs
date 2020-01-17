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

-- An example syntax type that we'll eventually replace with something linked to
-- Syn.
data Exp = Var RawName
         | Con Con
         | App Exp Exp
         | Abs RawName Exp
         | Case Exp [Alt]
         | Let RawName Exp Exp
         | LetA RawName Scheme Exp Exp
         | Hole RawName
         | TupleLit [Exp]
         | ListLit [Exp]
         | IntLit Int
         | StringLit String [(Exp, String)]
         deriving (Eq, Show)

-- Exp with type annotation
data ExpT = VarT RawName Type
          | ConT Con
          | AppT ExpT ExpT
          | AbsT RawName Type ExpT
          | CaseT ExpT [AltT] Type
          | LetT RawName ExpT ExpT Type
          | LetAT RawName Scheme ExpT ExpT Type
          | HoleT RawName Type
          | TupleLitT [ExpT] Type
          | ListLitT [ExpT] Type
          | IntLitT Int Type
          | StringLitT String [(ExpT, String)] Type
         deriving (Eq, Show)

instance Sub ExpT where
  sub s (VarT n v         ) = VarT n (sub s v)
  sub _ (ConT c           ) = ConT c
  sub s (AppT a b         ) = AppT (sub s a) (sub s b)
  sub s (AbsT  x t    e   ) = AbsT x (sub s t) (sub s e)
  sub s (CaseT e alts t   ) = CaseT (sub s e) (map (sub s) alts) (sub s t)
  sub s (LetT x e b t     ) = LetT x (sub s e) (sub s b) (sub s t)
  sub s (LetAT x sch e b t) = LetAT x sch (sub s e) (sub s b) (sub s t)
  sub s (HoleT     n  t   ) = HoleT n (sub s t)
  sub s (TupleLitT es t   ) = TupleLitT (map (sub s) es) (sub s t)
  sub s (ListLitT  es t   ) = ListLitT (map (sub s) es) (sub s t)
  sub s (IntLitT   i  t   ) = IntLitT i (sub s t)
  sub s (StringLitT p cs t) = StringLitT p (mapFst (sub s) cs) (sub s t)

-- [RawName] are the variables bound by the case branch
data Alt = Alt Pat Exp
  deriving (Eq, Show)

data AltT = AltT Pat ExpT
  deriving (Eq, Show)

instance Sub AltT where
  sub s (AltT p e) = AltT p (sub s e)

-- Notice that patterns aren't inductive: this simplifies constraint generation.
-- The current plan is to desugar surface pattern syntax to nested case
-- expressions prior to type checking.
data Pat = ConPat Con [RawName] -- T a b c
         | VarPat RawName             -- a
         | WildPat
        deriving (Eq, Show)

-- Note: raw data constructors have the following type (a Scheme):
-- Forall [a, b, ..] [] t
-- where t has the form m -> n -> ... -> T a b ..

newtype Con = C RawName
  deriving (Eq, Show)

-- The Var will always be rigid type variables (I think)
data Scheme = Forall [Var] Constraint Type
  deriving (Eq, Show)

instance Vars Scheme where
  fuv (Forall tvars c t) = fuv c <> fuv t \\ Set.fromList tvars

type Env = Map RawName Scheme

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
  let t = TTuple elemTypes
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
findConTypeInAlts env (Alt (ConPat (C name) _) _ : alts) =
  case Map.lookup name env of
    Just t  -> Just t
    Nothing -> findConTypeInAlts env alts
findConTypeInAlts env (_ : alts) = findConTypeInAlts env alts

-- beta: a uvar representing the type of the whole case expression
-- ys:   uvars for each argument to the type constructor of the scrutinee
genAlt :: Env -> Type -> Type -> [Var] -> Alt -> GenerateM (AltT, CConstraint)
genAlt env beta _ ys (Alt (ConPat (C k) xi) e) = case Map.lookup k env of
  Nothing -> do
    a         <- TVar <$> fresh
    (e, _, _) <- generate env e
    pure (AltT (ConPat (C k) xi) e, Simple (a :~: TCon k []))
  Just (Forall as _ kt) -> do
    -- construct substitution
    let subst = zip as (map TVar ys)
    let us    = init (unfoldFnType kt)
    let us'   = map (Forall [] CNil . sub subst) us
    let env'  = Map.fromList (zip xi us') <> env
    -- check ei under assumption that all xi have type [ys/as]t
    (e, ti, ci) <- generate env' e
    let c' = ci <> Simple (ti :~: beta)
    pure (AltT (ConPat (C k) xi) e, c')

genAlt env beta scrutTy _ (Alt (VarPat x) e) = do
  u <- TVar <$> fresh
  -- check e under the assumption that x has type u
  let env' = Map.insert x (Forall [] CNil u) env
  (e, t, c) <- generate env' e
  -- require u ~ the type of the scrutinee
  let c' = Simple (u :~: scrutTy)
  pure (AltT (VarPat x) e, c <> c' <> Simple (t :~: beta))

genAlt env beta _ _ (Alt WildPat e) = do
  -- The wildcard can't be used inside e, so we don't need to extend the
  -- environment.
  (e, t, c) <- generate env e
  pure (AltT WildPat e, c <> Simple (t :~: beta))

-- Converts a -> b -> c into [a, b, c]
unfoldFnType :: Type -> [Type]
unfoldFnType (TCon "->" [x, y]) = x : unfoldFnType y
unfoldFnType t                  = [t]
