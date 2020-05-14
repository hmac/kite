-- The constraint solver
--
-- Based on the solver described in:
--   Modular Type Inference with Local Assumptions
--   (Vytiniotis, Peyton Jones, Schrijvers, Sulzmann)
--
-- Currently only works with simple equalities, but that's enough to handle
-- everything in Lam except for typeclasses.
--
-- To deal with typeclasses, we need to add the following:
-- Two more cases for interact: EQDICT (done), DDICT (done)
-- Two more cases for canon:    DFLATW and DFLATG (done)
-- the SIMPLIFY rule (except for SEQFEQ and SFEQFEQ) (done)
-- the TOPREACT rule (except for FINST)

module Constraint.Solve
  ( solveC
  , solveDebug
  )
where

import           Util
import           Data.Name

import qualified Data.Set                      as Set
import           Data.Set                       ( Set
                                                , (\\)
                                                , member
                                                )

import           Control.Monad.State.Strict
import           Constraint

import           Prelude                 hiding ( interact )

-- This is the quadruple described in §7.3
-- ( touchable variables
-- , flattening substitution
-- , given constraints
-- , wanted constraints
-- , fresh type variable source
-- )
type Quad = (Set Var, Subst, Constraint, Constraint, Int)
type Solve = State Quad

fresh :: Solve Var
fresh = do
  (t, s, g, w, k) <- get
  put (t, s, g, w, k + 1)
  let var = U (Local (Name (show k)))
  pure var

-- See fig. 14
solveC
  :: AxiomScheme
  -> Set Var
  -> Constraint
  -> CConstraint
  -> Either Error (Constraint, Subst)
solveC axs touchables given wanted =
  case solve axs (touchables, mempty, given, simple wanted, 0) of
    Left err -> Left err
    Right (residual, subst) ->
      -- All implication constraints should be completely solvable
      let implications = implic (sub subst wanted)
      in  do
            results <- mapM
              (\(vars, q, cc) -> do
                (cs, s) <- solveC axs vars (q <> given <> residual) cc
                pure (cs, s)
              )
              implications
            case mconcat (map fst results) of
              CNil  -> Right (residual, subst <> mconcat (map snd results))
              impls -> Left (UnsolvedConstraints impls)

-- This is the actual top level solver function
-- Given a set of simple constraints it returns a substitution and any residual
-- constraints
solve :: AxiomScheme -> Quad -> Either Error (Constraint, Subst)
solve axs input = case rewriteAll axs input of
  Left err -> Left err
  -- See §7.5 for details
  Right (vars, _subst, _given, wanted, _) ->
    let partitionPred (TVar b :~: t     ) = b `elem` vars && b `notElem` fuv t
        partitionPred (t      :~: TVar b) = b `elem` vars && b `notElem` fuv t
        partitionPred _                   = False
        (epsilon, residual) = partitionConstraint partitionPred wanted

        toTuple (TVar b :~: t     ) = (b, t)
        toTuple (t      :~: TVar b) = (b, t)
        toTuple q                   = error $ "Unexpected constraint " <> show q
        subst = nubOrdOn fst $ mapConstraint toTuple epsilon
    in  Right (sub subst residual, subst)

-- Solve a set of constraints
-- Repeatedly applies rewrite rules until there's nothing left to do
rewriteAll :: AxiomScheme -> Quad -> Either Error Quad
rewriteAll axs quad@(_, _, _, wanted, _) = case applyRewrite axs quad of
  Left err -> Left err
  Right quad'@(_, _, _, wanted', _) ->
    if sortConstraint wanted == sortConstraint wanted'
      then Right quad'
      else rewriteAll axs quad'

-- Like solve but shows the solving history
solveDebug :: AxiomScheme -> Quad -> Either Error [Quad]
solveDebug axs q = go [q] q
 where
  go hist quad@(_, _, _, wanted, _) = case applyRewrite axs quad of
    Left err -> Left err
    Right quad'@(_, _, _, wanted', _) ->
      if sortConstraint wanted == sortConstraint wanted'
        then Right (quad' : hist)
        else go (quad' : hist) quad'

run :: Solve (Either Error ()) -> Quad -> Either Error Quad
run f c = case runState f c of
  (Right (), c') -> Right c'
  (Left  e , _ ) -> Left e

-- Apply a round of rewriting
applyRewrite :: AxiomScheme -> Quad -> Either Error Quad
applyRewrite axs = run
  (  canonM Given
  >> canonM Wanted
  >> interactM Given
  >> interactM Wanted
  >> simplifyM
  >> topreactM Given  axs
  >> topreactM Wanted axs
  )

data Domain = Given | Wanted deriving (Eq, Show)

-- | Pick each unique pair of constraints and attempt to interact them to a single
-- result. Stop when we get a successful reaction.
interactM :: Domain -> Solve (Either Error ())
interactM dom = do
  (vars, subst, given, wanted, k) <- get
  let constraints = case dom of
        Given  -> given
        Wanted -> wanted
  case
      firstJust (map interactEach (focusPairs (flattenConstraint constraints)))
    of
      Just constraints' -> do
        case dom of
          Given  -> put (vars, subst, mconcat constraints', wanted, k)
          Wanted -> put (vars, subst, given, mconcat constraints', k)
        pure $ Right ()
      Nothing -> pure $ Right ()
 where
  -- Try to interact the constraint with each one in the list.
  -- If a match is found, replace the two reactants with the result
  -- If no match is found, return the original list of constraints
  interactEach :: (Constraint, Constraint, [Constraint]) -> Maybe [Constraint]
  interactEach (a, b, cs) = case interact a b of
    Nothing -> Nothing
    Just c  -> Just (c : cs)

-- | Use each given constraint to attempt to simplify each wanted constraint
simplifyM :: Solve (Either Error ())
simplifyM = do
  (vars, subst, given, wanted, k) <- get
  let wanteds  = flatten wanted
      givens   = flatten given
      wanteds' = foldl (\ws g -> map (simplify g) ws) wanteds givens
  put (vars, subst, given, mconcat wanteds', k)
  pure (Right ())

-- | Run canon on each given constraint, then the same on each wanted constraint.
canonM :: Domain -> Solve (Either Error ())
canonM Given = do
  (vars, subst, given, wanted, k) <- get
  case canonAll (flattenConstraint given) of
    Left  err    -> pure $ Left err
    Right given' -> do
      put (vars, subst, mconcat given', wanted, k)
      pure $ Right ()
canonM Wanted = do
  (vars, subst, given, wanted, k) <- get
  case canonAll (flattenConstraint wanted) of
    Left  err     -> pure $ Left err
    Right wanted' -> do
      put (vars, subst, given, mconcat wanted', k)
      pure $ Right ()

canonAll :: [Constraint] -> Either Error [Constraint]
canonAll []       = Right []
canonAll (c : cs) = case canon c of
  Left  err -> Left err
  Right c'  -> (flatten c' ++) <$> canonAll cs

flatten :: Constraint -> [Constraint]
flatten (a :^: b) = a : flatten b
flatten c         = [c]

-- Canonicalise a constraint
canon :: Constraint -> Either Error Constraint
-- REFL: Reflexive equalities can be ignored
canon (a :~: b) | a == b = pure mempty

-- TAPP: Equalities between function types can be decomposed to equalities between
-- their components
canon (TApp (TCon k) as :~: TApp (TCon k') bs) | k == k' = pure (as :~: bs)
-- Note: This is quite naive. It can't deal with cases where the number of
-- nested applications differs between the types, which can happen if there's a
-- variable in place of an application.
canon (t1@TApp{} :~: t2@TApp{}) =
  let t1s = unfoldTyApp t1
      t2s = unfoldTyApp t2
  in  pure $ if length t1s == length t2s
        then foldl (:^:) CNil (zipWith (:~:) t1s t2s)
        else t1 :~: t2

-- TDEC: Equalities between identical constructors can be dropped
-- FAILDEC: Equalities between constructor types must have the same constructor
canon (TCon k :~: TCon k') =
  if k == k' then pure CNil else Left (ConstructorMismatch (TCon k) (TCon k'))

-- Equalities between records with identical field labels can be decomposed to
-- equality on their fields
canon (TRecord fs1 :~: TRecord fs2) | map fst fs1 == map fst fs2 =
  pure (foldl (:^:) CNil (zipWith (:~:) (map snd fs1) (map snd fs2)))

-- HasField constraints on a TRecord can be simplified into equality on the
-- field type.
canon (HasField (TRecord fields) l t) = case lookup l fields of
  Just t' -> pure (t :~: t')
  Nothing -> Left $ RecordDoesNotHaveLabel (TRecord fields) l

-- OCCCHECK: a type variable cannot be equal to a type containing that variable
canon (v@(TVar _) :~: t) | v /= t && t `contains` v =
  Left $ OccursCheckFailure v t

-- ORIENT: Flip an equality around if desirable
canon (a :~: b) | canonCompare a b == GT = pure (b :~: a)

-- DFLATW, DFLATG, FFLATWL, FFLATWR, FFLATGL, FFLATGR: the flattening rules
-- These are all omitted because they deal with type function application.
-- Type level functions arise from type families and the like, and Lam does not
-- have these.

-- Custom rule: CNil ^ c = c
canon (CNil :^: c   )                    = pure c
canon (c    :^: CNil)                    = pure c

-- Flattening rules only apply to type classes and type families, so are
-- omitted.
canon c                                  = pure c

-- Combine two canonical constraints into one
interact :: Constraint -> Constraint -> Maybe Constraint
-- EQSAME: Two equalities with the same LHS are combined to equate the RHS.
interact c1@(TVar v1 :~: t1) c2@(TVar v2 :~: t2)
  | v1 == v2 && isCanonical c1 && isCanonical c2
  = Just $ (TVar v1 :~: t1) :^: (t1 :~: t2)

-- EQDIFF: One equality can be substituted into the other. We rely the ORIENT
-- rule in prior canonicalisation to ensure this makes progress.
interact c1@(TVar v1 :~: t1) c2@(TVar v2 :~: t2)
  | v1 `member` ftv t2 && isCanonical c1 && isCanonical c2
  = Just $ (TVar v1 :~: t1) :^: (TVar v2 :~: sub [(v1, t1)] t2)

-- EQAPP (invented): We can substitute an equality into any other equality
interact c1@(TVar v1 :~: t1) (t2 :~: t3)
  | ((v1 `member` ftv t2) || (v1 `member` ftv t3)) && isCanonical c1
  = Just $ c1 :^: (sub [(v1, t1)] t2 :~: sub [(v1, t1)] t3)

-- EQDICT: We can substitute an equality into a typeclass constraint.
interact c1@(TVar v1 :~: t1) (Inst className tys)
  | v1 `member` ftv tys && isCanonical c1 = Just $ (TVar v1 :~: t1) :^: Inst
    className
    (sub [(v1, t1)] tys)

-- EQRECORD (invented): We can substitute an equality into a HasField constraint
interact c1@(TVar v1 :~: t1) (HasField r l t)
  | v1 `member` ftv r && isCanonical c1
  = Just $ (TVar v1 :~: t1) :^: HasField (sub [(v1, t1)] r) l t
  | v1 `member` ftv t && isCanonical c1
  = Just $ (TVar v1 :~: t1) :^: HasField r l (sub [(v1, t1)] t)

-- DDICT: We can drop duplicate typeclass constraints.
interact i1@(Inst _ _) i2@(Inst _ _) | i1 == i2 = Just i1

-- Redundant cases: drop CNil
interact CNil c                                 = Just c
interact c    CNil                              = Just c

-- If no rules match, signal failure
interact _    _                                 = Nothing

-- Use a given constraint to simplify a wanted constraint
-- e.g. given:   a ~ Int
--     wanted:   a ~ Int
--   produces: Int ~ Int
-- These rules are all similar to the interact rules above
simplify :: Constraint -> Constraint -> Constraint
-- SEQSAME
simplify c1@(TVar v1 :~: t1) c2@(TVar v2 :~: t2)
  | v1 == v2 && isCanonical c1 && isCanonical c2 = t1 :~: t2

-- SEQDIFF
simplify c1@(TVar v1 :~: t1) c2@(TVar v2 :~: t2)
  | v1 `member` ftv t2 && isCanonical c1 && isCanonical c2
  = TVar v2 :~: sub [(v1, t1)] t2

-- SEQDICT
simplify (TVar v1 :~: t1) (Inst className tys)
  | isCanonical (TVar v1 :~: t1) && v1 `member` ftv tys = Inst
    className
    (sub [(v1, t1)] tys)

-- SDDICTG
simplify i1@Inst{} i2@Inst{} | i1 == i2 = mempty

-- If no rules match, return the wanted constraint unchanged
simplify _ w                            = w

-- | Interact a constraint with an axiom from the top level axiom scheme
-- This is where typeclass instances start to get introduced.
topreactM :: Domain -> AxiomScheme -> Solve (Either Error ())
topreactM Given axs = do
  (_, _, given, _, _) <- get
  errors              <-
    concat <$> mapM (\ax -> mapM (topreactDINSTG ax) (flatten given)) axs
  case firstLeft errors of
    Just err -> pure (Left err)
    _        -> pure (Right ())
topreactM Wanted axs = do
  forM_ axs $ \ax -> do
    (vars, subst, given, wanted, _) <- get
    (wanted', vars')                <- unzip <$> forM
      (flatten wanted)
      (\w -> do
        res <- topreactDINSTW ax w
        case res of
          Nothing      -> pure (w, mempty)
          Just (v, w') -> pure (w', v)
      )
    -- topreactDINSTW may have incremented k, so refetch it
    (_, _, _, _, k) <- get
    put (vars <> mconcat vars', subst, given, mconcat wanted', k)
  pure (Right ())

-- React wanted typeclass instances with top level axioms
topreactDINSTW :: Axiom -> Constraint -> Solve (Maybe (Set Var, Constraint))
topreactDINSTW (AForall as q (Inst cn1 ts0)) (Inst cn2 ts1) | cn1 == cn2 = do
  -- In order to react, we must find a substitution mapping ts0 to ts1
  -- For each variable b which is free in ts0 but not present in ts1, we must
  -- find the type in ts1 which it corresponds to. E.g if ts0 is [b] and ts1 is
  -- [Bool], then we want to find b->Bool.
  -- From this we can construct a substitution that will convert ts0 into t1.
  let bs = ftv ts0
      cs = Set.toList (as \\ bs)
  ys <- mapM (const fresh) cs
  let btys = map
        (\v -> (v, ) <$> firstJust (zipWith (findCounterpart v) ts0 ts1))
        (Set.toList bs)
  -- TODO: I don't think we should have any Nothings in btys - I think that's a
  -- sign that we have two typeclass instances for the same class with different
  -- structure, which probably indicates a bug.
  let subst = zip cs (map TVar ys) <> catMaybes btys
  if sub subst ts0 == ts1
    then pure $ Just (Set.fromList ys, sub subst q)
    else pure Nothing
topreactDINSTW _ _ = pure Nothing

-- | React given typeclass instances with top level axioms
-- This is just a consistency check, really.
topreactDINSTG :: Axiom -> Constraint -> Solve (Either Error ())
topreactDINSTG (AForall as _q (Inst cn1 ts0)) (Inst cn2 ts1) | cn1 == cn2 = do
  let atys = map
        (\v -> (v, ) <$> firstJust (zipWith (findCounterpart v) ts0 ts1))
        (Set.toList as)
  let subst = catMaybes atys
  if sub subst ts0 == ts1
    then pure $ Left OverlappingTypeclassInstances
    else pure $ Right ()
topreactDINSTG _ _ = pure (Right ())

contains :: Type -> Type -> Bool
contains a b | a == b = True
contains (TApp a b) t = any (`contains` t) [a, b]
contains _          _ = False

canonCompare :: Type -> Type -> Ordering
canonCompare (TVar (U _)) (TVar (R _)) = LT
canonCompare (TVar (R _)) (TVar (U _)) = GT
canonCompare (TVar a    ) (TVar b    ) = compare a b
canonCompare _            (TCon _    ) = LT
canonCompare (TCon _)     _            = GT
canonCompare _            (TApp _ _)   = LT
canonCompare (TApp _ _)   _            = GT
canonCompare _            _            = EQ

-- Fig. 20
-- A constraint is canonical if either:
-- - it is a typeclass instance, or
-- - it has the form (v ~ t) and v is not free in t
isCanonical :: Constraint -> Bool
isCanonical Inst{} = True
isCanonical (TVar v :~: t) | v `member` ftv t = False
                           | otherwise        = True
isCanonical _ = False

-- | Given a variable, a type containing that variable, and another
-- type which has had the variable substituted for a type, attempt to find
-- the type which was substituted. We do this by walking both types
-- simultaneously until we find the variable in the left type, and return
-- the corresponding part of the right type.
findCounterpart :: Var -> Type -> Type -> Maybe Type
findCounterpart v (TVar v') t | v == v'     = Just t
findCounterpart v (TApp a1 b1) (TApp a2 b2) = case findCounterpart v a1 a2 of
  Just t  -> Just t
  Nothing -> findCounterpart v b1 b2
findCounterpart _ _ _ = Nothing

-- A list of each element in the given list paired with the remaining elements
focus :: [a] -> [(a, [a])]
focus = go []
 where
  go _  []       = []
  go ys (x : xs) = (x, reverse ys ++ xs) : go (x : ys) xs

focusPairs :: [a] -> [(a, a, [a])]
focusPairs xs =
  concatMap (\(y, ys) -> map (\(z, zs) -> (y, z, zs)) (focus ys)) (focus xs)

-- | Extract the first Just value from a list
firstJust :: [Maybe a] -> Maybe a
firstJust []             = Nothing
firstJust (Just x  : _ ) = Just x
firstJust (Nothing : xs) = firstJust xs

-- | Extract the first Left value from a list
firstLeft :: [Either a b] -> Maybe a
firstLeft []            = Nothing
firstLeft (Left x : _ ) = Just x
firstLeft (_      : xs) = firstLeft xs

-- | Unfold a nested type application
-- f a b c ==> [f, a, b, c]
unfoldTyApp :: Type -> [Type]
unfoldTyApp (TApp a b) = unfoldTyApp a <> [b]
unfoldTyApp t          = [t]
