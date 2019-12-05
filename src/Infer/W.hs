module Infer.W
  ( runInfer'
  , Env
  , Scheme(..)
  )
where

-- This module performs Hindley-Milner type inference on Core terms

import           Desugar
import           Syntax                         ( Ty(..)
                                                , Name(..)
                                                )
import           Infer.NameGen

import qualified Data.HashMap.Strict           as Map
import           Data.HashMap.Strict            ( HashMap )
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as Set
import           Data.Maybe                     ( fromMaybe )

data Scheme = Forall [Name] Ty deriving (Eq, Show)

type Env = HashMap Name Scheme

type Sub = HashMap Name Ty

instantiate :: Scheme -> NameGen Ty
instantiate (Forall vars t) = do
  newVars <- traverse (const newVar) vars
  let sub = Map.fromList (zip vars newVars)
  pure (applyTy sub t)

-- TODO: other types
applyTy :: Sub -> Ty -> Ty
applyTy s (TyVar v  ) = fromMaybe (TyVar v) (Map.lookup v s)
applyTy s (TyArr a b) = TyArr (applyTy s a) (applyTy s b)
applyTy _ t           = t

applyScheme :: Sub -> Scheme -> Scheme
applyScheme s (Forall vars t) =
  let s' = foldr Map.delete s vars in Forall vars (applyTy s' t)

applyEnv :: Sub -> Env -> Env
applyEnv s = fmap (applyScheme s)

newVar :: NameGen Ty
newVar = TyVar <$> genName

-- Most General Unifier
-- --------------------
-- A substitution s is a unifier of two types t1 and t2 if
--   apply s t1 = apply s t2.
-- A most general unifier, or mgu, of two such types is a unifier u with the
-- property that any other unifier s can be written as s′ <> u, for some
-- substitution s′.
mgu :: Ty -> Ty -> NameGen (Either String Sub)
mgu (TyArr a1 b1) (TyArr a2 b2) = do
  es1 <- mgu a1 a2
  case es1 of
    Left  err -> pure $ Left err
    Right s1  -> do
      es2 <- mgu (applyTy s1 b1) (applyTy s1 b2)
      case es2 of
        Left  err -> pure $ Left err
        Right s2  -> pure $ Right (s1 <> s2)
mgu (TyVar v) t         = pure $ varBind v t
mgu t         (TyVar v) = pure $ varBind v t
mgu a b | a == b        = pure $ Right mempty
mgu t1 t2 = pure $ Left $ "Cannot unify " <> show t1 <> " and " <> show t2

fvTy :: Ty -> HashSet Name
fvTy (TyVar v  ) = Set.singleton v
fvTy (TyArr a b) = fvTy a <> fvTy b
fvTy _           = mempty

fvScheme :: Scheme -> HashSet Name
fvScheme (Forall vars t) = Set.difference (Set.fromList vars) (fvTy t)

fvEnv :: Env -> HashSet Name
fvEnv e = Set.unions $ map fvScheme (Map.elems e)

varBind :: Name -> Ty -> Either String Sub
varBind v t
  | t == TyVar v
  = Right mempty
  | -- don't bind a variable to itself
    v `elem` fvTy t
  = Left
    $  "cannot bind "
    <> show v
    <> " to "
    <> show t
    <> " because it references "
    <> show v
  | otherwise
  = Right $ Map.singleton v t

generalise :: Env -> Ty -> Scheme
generalise env t =
  let freeVars = Set.difference (fvTy t) (fvEnv env)
  in  Forall (Set.toList freeVars) t

infer :: Env -> Core -> NameGen (Either String (Sub, Ty))
infer _   (IntLit    _) = pure $ Right (mempty, TyInt)
infer _   (FloatLit  _) = pure $ Right (mempty, TyFloat)
infer _   (StringLit _) = pure $ Right (mempty, TyString)
infer env (Var       v) = case Map.lookup v env of
  Just sigma -> do
    t <- instantiate sigma
    pure $ Right (mempty, t)
  Nothing -> pure $ Left ("Unbound variable: " <> show v)
infer env (Abs x e) = do
  tv <- newVar
  let env' = env <> Map.singleton x (Forall mempty tv)
  res <- infer env' e
  case res of
    Left  err      -> pure (Left err)
    Right (s1, t1) -> pure $ Right (s1, TyArr (applyTy s1 tv) t1)
infer env (App e1 e2) = do
  tv  <- newVar
  res <- infer env e1
  case res of
    Left  err      -> pure (Left err)
    Right (s1, t1) -> do
      res <- infer (applyEnv s1 env) e2
      case res of
        Left  err      -> pure (Left err)
        Right (s2, t2) -> do
          res <- mgu (applyTy s2 t1) (TyArr t2 tv)
          case res of
            Left  err -> pure $ Left err
            Right s3  -> pure $ Right (s3 <> s2 <> s1, applyTy s3 tv)
infer env (Let x e1 e2) = do
  res <- infer env e1
  case res of
    Left  err      -> pure $ Left err
    Right (s1, t1) -> do
      let t'   = generalise (applyEnv s1 env) t1
          env' = Map.insert x t' env
      res <- infer (applyEnv s1 env') e2
      case res of
        Left  err      -> pure $ Left err
        Right (s2, t2) -> pure $ Right (s1 <> s2, t2)

runInfer :: Env -> Core -> NameGen (Either String Ty)
runInfer env e = do
  res <- infer env e
  case res of
    Left  err    -> pure $ Left err
    Right (s, t) -> pure $ Right $ applyTy s t

runInfer' :: Env -> Core -> Either String Ty
runInfer' env e =
  let gen = Gen 0 (getVars e) in evalNameGen (runInfer env e) gen

getVars :: Core -> HashSet Name
getVars (Var v      ) = Set.singleton v
getVars (Abs x e    ) = Set.insert x (getVars e)
getVars (App a b    ) = getVars a <> getVars b
getVars (Let x e1 e2) = Set.insert x (getVars e1 <> getVars e2)
-- TODO: the rest
getVars e             = mempty
