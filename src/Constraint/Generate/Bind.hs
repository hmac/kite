module Constraint.Generate.Bind where

import           Constraint
import           Constraint.Generate
import           Constraint.Generate.M
import           Constraint.Solve
import           Data.Name

import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

-- Generate constraints for top level function bindings

-- We assume that patterns & multi declarations have been desugared to case
-- expressions.
-- e.g.               f []       = 0
--                    f (_ : xs) = 1 + f xs
--
-- is represented as  f l = case l of
--                            [] -> 0
--                            (_ : xs) = 1 + f xs
data Bind = Bind RawName Exp (Maybe Scheme)
  deriving (Show, Eq)

data BindT = BindT RawName ExpT Scheme
  deriving (Show, Eq)

generateBind :: Env -> Bind -> GenerateM (Either Error (BindT, Env))

-- Unannotated bind
-- See fig. 12
generateBind env (Bind name expr Nothing) = do
  (e, t, c) <- generate env expr
  let touchables = fuv t <> fuv c
  case solveC touchables c of
    Left  err        -> pure $ Left err
    Right (q, subst) -> do
      let t' = sub subst t
      let e' = sub subst e
      -- bind all the free unification vars in q and t' as rigid vars in the
      -- type of the function
      -- this is the reverse of the usual substitutions: uvar -> tvar
      tysubst <- mapM (\v -> (v, ) . TVar <$> freshR)
                      (Set.toList (fuv t' <> fuv q))
      let ty = Forall (map fst tysubst) (sub tysubst q) (sub tysubst t')
      pure $ Right (BindT name (sub tysubst e') ty, Map.insert name ty env)

-- Annotated bind
generateBind env (Bind name expr (Just (Forall tvars q t))) = do
  (e, v, c) <- generate env expr
  let touchables = fuv v <> fuv c
  case solveC touchables (c <> Simple (v :~: t)) of
    Left  err           -> pure $ Left err
    Right (CNil, subst) -> do
      let e' = sub subst e
      let ty = Forall tvars q t
      pure $ Right (BindT name e' ty, Map.insert name ty env)
    Right (residual, _) -> pure $ Left (UnsolvedConstraints residual)
