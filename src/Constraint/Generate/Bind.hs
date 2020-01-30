module Constraint.Generate.Bind where

import           Canonical                      ( Name )
import           Constraint
import           Constraint.Expr
import           Constraint.Generate
import           Constraint.Generate.Pattern
import           Constraint.Generate.M
import           Constraint.Solve
import           Util

import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

-- Generate constraints for top level function bindings

data Bind = Bind Name (Maybe Scheme) [([Pattern], Exp)]
  deriving (Show, Eq)

data BindT = BindT Name [([Pattern], ExpT)] Scheme
  deriving (Show, Eq)

-- Fig. 12
generateBind :: Env -> Bind -> GenerateM (Either Error (Env, BindT))
generateBind _ (Bind _ _ equations) | not (sameNumberOfPatterns equations) =
  pure $ Left EquationsHaveDifferentNumberOfPatterns
generateBind env (Bind name annotation equations) = do
  (es, eqTypes, cs) <- unzip3 <$> mapM (generateMultiEquation env) equations
  (beta, allEqsEq)  <- do
    beta <- TVar <$> fresh
    pure (beta, generateAllEqualConstraint beta eqTypes)
  let annotationConstraint = case annotation of
        (Just (Forall tvars q t)) -> q <> beta :~: t
        Nothing                   -> CNil
  -- TODO: is it ok for all of these to be touchable?
  let touchables  = fuv eqTypes <> fuv beta <> fuv cs
  let constraints = mconcat cs <> Simple (allEqsEq <> annotationConstraint)
  case solveC touchables constraints of
    Left  err        -> pure $ Left err
    Right (q, subst) -> do
      -- At this point, q should only contain typeclass constraints.
      -- We should have solved all the equality constraints.
      if q /= mempty
        then pure (Left (UnsolvedConstraints q))
        else do

          -- apply the substitution to remove all resolved unification vars
          let eqTypes' = map (sub subst) eqTypes
          let exps'    = map (sub subst) es
          case annotation of
            Just bindTy ->
              let
                bind = BindT
                  name
                  (zipWith (\e' (p, _) -> (p, e')) exps' equations)
                  bindTy
              in  pure $ Right (Map.insert name bindTy env, bind)
            Nothing -> do
              -- bind all the free unification vars in the types and residual as
              -- rigid vars in the type of the function
              -- this is the reverse of the usual substitutions: uvar -> tvar
              tysubst <- mapM (\v -> (v, ) . TVar <$> freshR)
                              (Set.toList (fuv eqTypes' <> fuv q))
              -- Since everything is solved, we can take the first equation type
              -- and use that
              let eqType = head eqTypes'
              let bindTy = Forall (map (\(_, TVar v) -> v) tysubst)
                                  (sub tysubst q)
                                  (sub tysubst eqType)
              let exps'' = map (sub tysubst) exps'
              let
                bind = BindT
                  name
                  (zipWith (\e' (p, _) -> (p, e')) exps'' equations)
                  bindTy
              pure $ Right (Map.insert name bindTy env, bind)

-- Generate constraints for a single branch of a multi-equation function
-- definition. Branches can have 0+ patterns.
-- e.g.
--   five = 5
--
--   id x = x
--
--   const x y = x
--
--   and True True = True
--   and _    _    = False
generateMultiEquation
  :: Env -> ([Pattern], Exp) -> GenerateM (ExpT, Type, CConstraint)
generateMultiEquation env (pats, expr) = do
  (patTypes, patCs, envs) <-
    unzip3
      <$> mapM (\pat -> fresh >>= \t -> generatePattern env (TVar t) pat) pats
  let env' = mconcat envs
  (e, expType, expC) <- generate env' expr
  let eqType = foldr fn expType patTypes
  pure (e, eqType, mconcat patCs <> expC)

sameNumberOfPatterns :: [([Pattern], a)] -> Bool
sameNumberOfPatterns = allEqual . map (length . fst)

allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual (x : xs) = all (== x) xs
