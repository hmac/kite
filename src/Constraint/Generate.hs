-- The constraint generator

{-# LANGUAGE TupleSections #-}
module Constraint.Generate where

import           Control.Monad                  ( forM )
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import qualified Data.Set                      as Set
import           Data.Name
import           NameGen                        ( NameGen )
import qualified NameGen
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
         deriving (Eq, Show)

-- [RawName] are the variables bound by the case branch
data Alt = Alt Con [RawName] Exp
  deriving (Eq, Show)

-- Note: raw data constructors have the following type (a Scheme):
-- Forall [a, b, ..] [] t
-- where t has the form m -> n -> ... -> T a b ..

newtype Con = C RawName
  deriving (Eq, Show)

-- The Var will always be rigid type variables (I think)
data Scheme = Forall [Var] Constraint Type
  deriving (Eq, Show)

type Subst a b = [(a, b)]

run :: NameGen a -> a
run = NameGen.run

-- Generate a fresh unification variable
fresh :: NameGen Var
fresh = NameGen.freshM (U . Name . show)

-- TODO: use Data.Map
type Env = [(RawName, Scheme)]

generate :: Env -> Exp -> NameGen (Type, CConstraint)
-- VARCON
generate env (Var name) = case lookup name env of
  Just (Forall tvars c t) -> do
    subst <- mapM (\tv -> (tv, ) <$> fresh) tvars
    let t' = substVarsT subst t
    let q' = substVarsQ subst c
    pure (t', Simple q')
  Nothing -> do
    a <- TVar <$> fresh
    pure (a, Simple CNil)
-- Data constructors are treated identically to variables
generate env (Con (C n)) = generate env (Var n)
-- APP
generate env (App e1 e2) = do
  (t1, c1) <- generate env e1
  (t2, c2) <- generate env e2
  a        <- TVar <$> fresh
  let funcConstraint = Simple $ t1 :~: (t2 `fn` a)
  pure (a, c1 :^^: (c2 :^^: funcConstraint))
-- ABS
generate env (Abs x e) = do
  a      <- TVar <$> fresh
  (t, c) <- generate ((x, Forall [] CNil a) : env) e
  pure (a `fn` t, c)
-- LET
generate env (Let x e1 e2) = do
  (t1, c1) <- generate env e1
  (t2, c2) <- generate ((x, Forall [] CNil t1) : env) e2
  pure (t2, c1 :^^: c2)
-- LETA
generate env (LetA x (Forall [] CNil t1) e1 e2) = do
  (t , c1) <- generate env e1
  (t2, c2) <- generate ((x, Forall [] CNil t1) : env) e2
  pure (t2, c1 :^^: (c2 :^^: Simple (t :~: t1)))
-- GLETA
generate env (LetA x s1@(Forall tvars q1 t1) e1 e2) = do
  (t, c) <- generate env e1
  let betas = Set.toList $ fuvT t <> fuvCC c \\ fuvEnv env
  let c1    = E betas q1 (c :^^: Simple (t :~: t1))
  (t2, c2) <- generate ((x, s1) : env) e2
  pure (t2, c1 :^^: c2)

-- CASE
-- We use the simplified version from Fig 6 because Lam doesn't have GADTs. If
-- it turns out that typeclasses need the more complex version, this will need
-- to be changed.

-- Lam doesn't support empty cases. If there are no cases, just return a new
-- unification variable, which will cause a type error
generate env (Case e []) = do
  a <- fresh
  pure (TVar a, Simple CNil)
generate env (Case e (alt : alts)) = do
  (t, c) <- generate env e
  -- get the tycon from the constructor in the first alt
  let (Alt (C k) _ _) = alt
  case lookup k env of
    Nothing -> do
      a <- TVar <$> fresh
      pure (a, Simple CNil)
    Just (Forall tvars _ tk) -> do
      let (TCon tyname _) = last (unfoldFnType tk)
      ys <- mapM (const fresh) tvars
      let c' = Simple (TCon tyname (map TVar ys) :~: t) :^^: c
      beta <- TVar <$> fresh
      cis  <- forM (alt : alts) $ \(Alt (C k) xi ei) -> case lookup k env of
        Nothing -> do
          a <- TVar <$> fresh
          pure $ Simple (a :~: TCon k [])
        -- find k in env, get tyvars as
        Just (Forall as _ kt) -> do
          -- construct substitution
          let subst = zip as ys
          let us = reverse (tail (reverse (unfoldFnType kt)))
          let us' = map (Forall [] CNil . substVarsT subst) us
          let env'  = zip xi us' ++ env
          -- check ei under assumption that all xi have type [ys/as]t
          (ti, ci) <- generate env' ei
          pure $ ci :^^: Simple (ti :~: beta)
      -- combine all the generated constraints together
      let c'' = c' :^^: foldl (:^^:) (Simple CNil) cis
      pure (beta, c'')

fn :: Type -> Type -> Type
a `fn` b = TCon "->" [a, b]

-- Converts a -> b -> c into [a, b, c]
unfoldFnType :: Type -> [Type]
unfoldFnType t = [t]

-- TODO: maybe a typeclass for these functions?
fuvT :: Type -> Set Var
fuvT (TVar (U v)) = Set.singleton (U v)
fuvT (TVar _    ) = mempty
fuvT (TCon _ ts ) = Set.unions (map fuvT ts)

fuvC :: Constraint -> Set Var
fuvC CNil      = mempty
fuvC (a :^: b) = fuvC a <> fuvC b
fuvC (t :~: v) = fuvT t <> fuvT v

fuvCC :: CConstraint -> Set Var
fuvCC (Simple c   ) = fuvC c
fuvCC (a :^^: b   ) = fuvCC a <> fuvCC b
fuvCC (E vars c cc) = fuvC c <> fuvCC cc \\ Set.fromList vars

fuvS :: Scheme -> Set Var
fuvS (Forall tvars c t) = fuvC c <> fuvT t \\ Set.fromList tvars

fuvEnv :: Env -> Set Var
fuvEnv env = Set.unions (map (fuvS . snd) env)

substVarsT :: Subst Var Var -> Type -> Type
substVarsT s (TVar v) = case lookup v s of
  Just v' -> TVar v'
  Nothing -> TVar v
substVarsT s (TCon n ts) = TCon n (map (substVarsT s) ts)

substVarsQ :: Subst Var Var -> Constraint -> Constraint
substVarsQ _ CNil      = CNil
substVarsQ s (a :^: b) = substVarsQ s a :^: substVarsQ s b
substVarsQ s (t :~: v) = substVarsT s t :~: substVarsT s v
