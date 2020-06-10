module Constraint.Generate.Bind where

import           Data.Name                      ( Name )
import           Constraint
import           Constraint.Expr
import           Constraint.Generate
import           Constraint.Generate.Pattern
import           Constraint.Generate.M
import           Constraint.Solve
import           Util
import           Syn                            ( Pattern_(..) )

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

-- Generate constraints for top level function bindings

data Bind = Bind Name (Maybe Scheme) [([Pattern], Exp)]
  deriving (Show, Eq)

data BindT = BindT Name [([Pattern], ExpT)] Scheme
  deriving (Show, Eq)

withLocation :: Name -> GenerateM Error a -> GenerateM LocatedError a
withLocation name = mapError (LocatedError name)

-- Fig. 12
generateBind
  :: AxiomScheme -> TypeEnv -> Bind -> GenerateM LocatedError (TypeEnv, BindT)
generateBind _ _ (Bind name _ equations)
  | not (sameNumberOfPatterns equations) = throwError
    (LocatedError name EquationsHaveDifferentNumberOfPatterns)
generateBind axs env (Bind name annotation equations) = withLocation name $ do
  -- Firstly, generate a fresh type variable for the whole binding, and extend
  -- the environment with it. This means any recursive reference to the binding
  -- will be correctly in scope.
  beta <- TVar <$> fresh
  let env' = Map.insert name (Forall [] beta) env
  (es, eqTypes, cs) <- unzip3 <$> mapM (generateMultiEquation env') equations
  let allEqsEq = generateAllEqualConstraint beta eqTypes
  let annWanted = case annotation of
        (Just (Forall _tvars t)) -> [beta :~: t]
        Nothing                  -> mempty
  -- TODO: is it ok for all of these to be touchable?
  let touchables  = fuv eqTypes <> fuv beta <> fuv cs
  let constraints = allEqsEq <> annWanted <> mconcat cs
  case solveC axs touchables mempty constraints of
    Left err                        -> throwError err
    -- At this point, q should only contain typeclass constraints.
    -- We should have solved all the equality constraints.
    Right (q, _subst) | q /= mempty -> throwError (UnsolvedConstraints q)
    Right (q, subst)                -> do
      -- apply the substitution to remove all resolved unification vars
      let eqTypes' = map (sub subst) eqTypes
      let exps'    = map (sub subst) es

      case annotation of
        Just bindTy -> do
          -- We should now have no remaining unification variables.
          let remainingUVars = fuv eqTypes' <> fuv exps'
          if remainingUVars /= mempty
            then throwError (UnsolvedUnificationVariables remainingUVars q)
            else
              let
                bind = BindT
                  name
                  (zipWith (\e' (p, _) -> (p, e')) exps' equations)
                  bindTy
                holes = concatMap findHoles exps'
              in
                case holes of
                  ((holeName, holeType) : _) -> throwError $ HoleFound
                    holeName
                    holeType
                    (bindingsWithType holeType env)
                  _ -> pure (Map.insert name bindTy env, bind)
        Nothing -> do
          -- bind all the free unification vars in the types and residual as
          -- rigid vars in the type of the function
          -- this is the reverse of the usual substitutions: uvar -> tvar
          tysubst <- Map.fromList <$> mapM
            (\v -> (v, ) . TVar <$> freshR)
            (Set.toList (fuv eqTypes' <> fuv exps' <> fuv q))
          -- Since everything is solved, we can take the first equation type
          -- and use that
          let
            getTVar t = case t of
              TVar v -> v
              t' ->
                error
                  $  "Constraint.Generate.Bind: expected TVar, found "
                  <> show t'
            bindTy = Forall (map getTVar (Map.elems tysubst))
                            (sub tysubst (head eqTypes'))
            exps'' = map (sub tysubst) exps'
            bind   = BindT name
                           (zipWith (\e' (p, _) -> (p, e')) exps'' equations)
                           bindTy
            holes = concatMap findHoles exps''
          case holes of
            ((holeName, holeType) : _) -> throwError
              $ HoleFound holeName holeType (bindingsWithType holeType env)
            _ -> pure (Map.insert name bindTy env, bind)

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
  :: TypeEnv -> ([Pattern], Exp) -> GenerateM Error (ExpT, Type, Constraints)
generateMultiEquation _ (pats, _) | hasDuplicates (patternVariables pats) =
  throwError DuplicatePatternVariables
generateMultiEquation env (pats, expr) = do
  (patTypes, patCs, envs) <-
    unzip3
      <$> mapM (\pat -> fresh >>= \t -> generatePattern env (TVar t) pat) pats
  let env' = env <> mconcat envs
  (e, expType, expC) <- generate env' expr
  let eqType = foldr fn expType patTypes
  pure (e, eqType, mconcat (expC : patCs))

sameNumberOfPatterns :: [([Pattern], a)] -> Bool
sameNumberOfPatterns = allEqual . map (length . fst)

patternVariables :: [Pattern] -> [Name]
patternVariables = concatMap f
 where
  f p = case p of
    VarPat x     -> [x]
    WildPat      -> []
    IntPat    _  -> []
    StringPat _  -> []
    BoolPat   _  -> []
    UnitPat      -> []
    TuplePat ps  -> patternVariables ps
    ListPat  ps  -> patternVariables ps
    ConsPat _ ps -> patternVariables ps

allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual (x : xs) = all (== x) xs

-- | Get all the holes in an expression
findHoles :: ExpT -> [(Name, Type)]
findHoles = go
 where
  go = \case
    HoleT name ty      -> [(name, ty)]
    AbsT  _    e       -> go e
    AppT  a    b       -> go a <> go b
    -- TODO: support holes in patterns
    CaseT s     alts _ -> go s <> concatMap (\(AltT _pat e) -> go e) alts
    LetT  binds e    _ -> go e <> concatMap (\(_, a) -> go a) binds
    LetAT _ _ a e _    -> go a <> go e
    TupleLitT es _     -> concatMap go es
    ListLitT  es _     -> concatMap go es
    StringLitT _ cs _  -> concatMap (\(e, _) -> go e) cs
    RecordT fs _       -> concatMap (\(_, e) -> go e) fs
    ProjectT e _  _    -> go e
    FCallT   _ es _    -> concatMap go es
    _                  -> mempty

-- | Return all the bindings in the environment with the given type
-- Currently only works for monomorphic bindings (i.e. types that don't bind any
-- type variables)
bindingsWithType :: Type -> TypeEnv -> Map Name Scheme
bindingsWithType ty = Map.filter (\(Forall _ t) -> t == ty)
