module Constraint.Generate.Bind where

import           Canonical                      ( Name )
import           Constraint
import           Constraint.Expr
import           Constraint.Generate
import           Constraint.Generate.M
import           Constraint.Solve
import           Data.Name
import           Util

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
data Bind = Bind Name (Maybe Scheme) [(Pattern, Exp)]
  deriving (Show, Eq)

data BindT = BindT Name [(Pattern, ExpT)] Scheme
  deriving (Show, Eq)

-- Fig. 12
generateBind :: Env -> Bind -> GenerateM (Either Error (BindT, Env))
generateBind env (Bind name annotation equations) = do
  (es, patTys, expTys, cs) <- unzip4 <$> mapM (generateEquation env) equations
  (beta1, allPatsEq)       <- do
    beta <- TVar <$> fresh
    pure (beta, generateAllEqualConstraint beta patTys)
  (beta2, allExpsEq) <- do
    beta <- TVar <$> fresh
    pure (beta, generateAllEqualConstraint beta expTys)
  let annotationConstraint = case annotation of
        (Just (Forall tvars q t)) -> q <> beta1 `fn` beta2 :~: t
        Nothing                   -> CNil
  -- TODO: is it ok for all of these to be touchable?
  let touchables = fuv [patTys, expTys] <> fuv [beta1, beta2] <> fuv cs
  case
      solveC
        touchables
        (mconcat cs <> Simple (allPatsEq <> allExpsEq <> annotationConstraint))
    of
      Left  err        -> pure $ Left err
      Right (q, subst) -> do
        -- At this point, q should only contain typeclass constraints.
        -- We should have solved all the equality constraints.

        -- apply the substitution to remove all resolved unification vars
        let expTys' = map (sub subst) expTys
        let patTys' = map (sub subst) patTys
        let exps'   = map (sub subst) es
        case annotation of
          Just bindTy ->
            let
              bind = BindT name
                           (zipWith (\e' (p, _) -> (p, e')) exps' equations)
                           bindTy
            in  pure $ Right (bind, Map.insert name bindTy env)
          Nothing -> do
            -- bind all the free unification vars in the types and residual as
            -- rigid vars in the type of the function
            -- this is the reverse of the usual substitutions: uvar -> tvar
            tysubst <- mapM (\v -> (v, ) . TVar <$> freshR)
                            (Set.toList (fuv patTys' <> fuv expTys' <> fuv q))
            -- Since everything is solved, we can take the first pattern and first exp
            -- types and use those
            let expTy = head expTys'
            let patTy = head patTys'
            let bindTy = Forall (map (\(_, TVar v) -> v) tysubst)
                                (sub tysubst q)
                                (sub tysubst (patTy `fn` expTy))
            let exps'' = map (sub tysubst) exps'
            let
              bind = BindT name
                           (zipWith (\e' (p, _) -> (p, e')) exps'' equations)
                           bindTy
            pure $ Right (bind, Map.insert name bindTy env)
