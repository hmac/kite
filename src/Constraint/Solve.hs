-- The constraint solver
--
-- Based on the solver described in:
--   Modular Type Inference with Local Assumptions
--   (Vytiniotis, Peyton Jones, Schrijvers, Sulzmann)
--
-- Currently only works with simple equalities, but that's enough to handle
-- everything in Lam.
--
-- Since Lam no longer supports typeclasses, this solver may be overkill.
-- Specifically, we can probably remove:
-- - wanted constraints
-- - flattening substitutions
-- - fresh type variable source

module Constraint.Solve
  ( solveC
  , solveDebug
  )
where

import           Util

import qualified Data.Map                      as Map
import           Data.Map                       ( singleton )
import           Data.Set                       ( Set
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
type Quad = (Set Var, Subst, Constraints, Constraints, Int)
type Solve = State Quad

-- See fig. 14
solveC
  :: AxiomScheme
  -> Set Var
  -> Constraints
  -> CConstraints
  -> Either Error (Constraints, Subst)
solveC axs touchables given wanted =
  case solve axs (touchables, mempty, given, concatMap simple wanted, 0) of
    Left err -> Left err
    Right (residual, subst) ->
      -- All implication constraints should be completely solvable
      let implications = concatMap implic (sub subst wanted)
      in  do
            results <- mapM
              (\(vars, q, cc) -> do
                (cs, s) <- solveC axs vars (q <> given <> residual) cc
                pure (cs, s)
              )
              implications
            case mconcat (map fst results) of
              []    -> Right (residual, subst <> mconcat (map snd results))
              impls -> Left (UnsolvedConstraints impls)

-- This is the actual top level solver function
-- Given a set of simple constraints it returns a substitution and any residual
-- constraints
solve :: AxiomScheme -> Quad -> Either Error (Constraints, Subst)
solve axs input = case rewriteAll axs input of
  Left err -> Left err
  -- See §7.5 for details
  Right (vars, _subst, _given, wanted, _) ->
    let partitionPred (TVar b :~: t     ) = b `elem` vars && b `notElem` fuv t
        partitionPred (t      :~: TVar b) = b `elem` vars && b `notElem` fuv t
        partitionPred _                   = False
        (epsilon, residual) = partition partitionPred wanted

        toTuple (TVar b :~: t     ) = (b, t)
        toTuple (t      :~: TVar b) = (b, t)
        toTuple q                   = error $ "Unexpected constraint " <> show q
        subst = Map.fromList $ nubOrdOn fst $ map toTuple epsilon
    in  Right (sub subst residual, subst)

-- Solve a set of constraints
-- Repeatedly applies rewrite rules until there's nothing left to do
rewriteAll :: AxiomScheme -> Quad -> Either Error Quad
rewriteAll axs quad@(_, _, _, wanted, _) = case applyRewrite axs quad of
  Left err -> Left err
  Right quad'@(_, _, _, wanted', _) ->
    if sort wanted == sort wanted' then Right quad' else rewriteAll axs quad'

-- Like solve but shows the solving history
solveDebug :: AxiomScheme -> Quad -> Either Error [Quad]
solveDebug axs q = go [q] q
 where
  go hist quad@(_, _, _, wanted, _) = case applyRewrite axs quad of
    Left  err                         -> Left err
    Right quad'@(_, _, _, wanted', _) -> if sort wanted == sort wanted'
      then Right (quad' : hist)
      else go (quad' : hist) quad'

run :: Solve (Either Error ()) -> Quad -> Either Error Quad
run f c = case runState f c of
  (Right (), c') -> Right c'
  (Left  e , _ ) -> Left e

-- Apply a round of rewriting
applyRewrite :: AxiomScheme -> Quad -> Either Error Quad
applyRewrite _axs = run
  (  canonM Given
  >> canonM Wanted
  >> interactM Given
  >> interactM Wanted
  >> simplifyM
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
  case firstJust (map interactEach (focusPairs constraints)) of
    Just constraints' -> do
      case dom of
        Given  -> put (vars, subst, constraints', wanted, k)
        Wanted -> put (vars, subst, given, constraints', k)
      pure $ Right ()
    Nothing -> pure $ Right ()
 where
  -- Try to interact the constraint with each one in the list.
  -- If a match is found, replace the two reactants with the result
  -- If no match is found, return the original list of constraints
  interactEach :: (Constraint, Constraint, Constraints) -> Maybe Constraints
  interactEach (a, b, cs) = case interact a b of
    Nothing -> Nothing
    Just c  -> Just (c ++ cs)

-- | Use each given constraint to attempt to simplify each wanted constraint
simplifyM :: Solve (Either Error ())
simplifyM = do
  (vars, subst, given, wanted, k) <- get
  let wanteds' = foldl (\ws g -> map (simplify g) ws) wanted given
  put (vars, subst, given, wanteds', k)
  pure (Right ())

-- | Run canon on each given constraint, then the same on each wanted constraint.
canonM :: Domain -> Solve (Either Error ())
canonM Given = do
  (vars, subst, given, wanted, k) <- get
  case canonAll given of
    Left  err    -> pure $ Left err
    Right given' -> do
      put (vars, subst, given', wanted, k)
      pure $ Right ()
canonM Wanted = do
  (vars, subst, given, wanted, k) <- get
  case canonAll wanted of
    Left  err     -> pure $ Left err
    Right wanted' -> do
      put (vars, subst, given, wanted', k)
      pure $ Right ()

canonAll :: Constraints -> Either Error Constraints
canonAll []       = Right []
canonAll (c : cs) = case canon c of
  Left  err -> Left err
  Right c'  -> (c' ++) <$> canonAll cs

-- Canonicalise a constraint
canon :: Constraint -> Either Error Constraints
-- REFL: Reflexive equalities can be ignored
canon (a :~: b) | a == b = pure mempty

-- TAPP: Equalities between function types can be decomposed to equalities between
-- their components
canon (TApp (TCon k) as :~: TApp (TCon k') bs) | k == k' = pure [as :~: bs]
-- Note: This is quite naive. It can't deal with cases where the number of
-- nested applications differs between the types, which can happen if there's a
-- variable in place of an application.
canon (t1@TApp{} :~: t2@TApp{}) =
  let t1s = unfoldTyApp t1
      t2s = unfoldTyApp t2
  in  pure $ if length t1s == length t2s
        then zipWith orient t1s t2s
        else [t1 :~: t2]

-- TDEC: Equalities between identical constructors can be dropped
-- FAILDEC: Equalities between constructor types must have the same constructor
canon (TCon k :~: TCon k') =
  if k == k' then pure [] else Left (ConstructorMismatch (TCon k) (TCon k'))

-- Equalities between records with identical field labels can be decomposed to
-- equality on their fields
canon (TRecord fs1 :~: TRecord fs2) | map fst fs1 == map fst fs2 =
  pure $ zipWith orient (map snd fs1) (map snd fs2)

-- HasField constraints on a TRecord can be simplified into equality on the
-- field type.
canon (HasField (TRecord fields) l t) = case lookup l fields of
  Just t' -> pure [t :~: t']
  Nothing -> Left $ RecordDoesNotHaveLabel (TRecord fields) l

-- OCCCHECK: a type variable cannot be equal to a type containing that variable
canon (v@(TVar _) :~: t) | v /= t && t `contains` v =
  Left $ OccursCheckFailure v t

-- ORIENT: Flip an equality around if desirable
canon (a :~: b) = pure [orient a b]

-- DFLATW, DFLATG, FFLATWL, FFLATWR, FFLATGL, FFLATGR: the flattening rules
-- These are all omitted because they deal with type function application.
-- Type level functions arise from type families and the like, and Lam does not
-- have these.

-- Flattening rules only apply to type classes and type families, so are
-- omitted.
canon c         = pure [c]

-- | Given an equality constraint, flip it around so it is oriented according to
-- the canonical ordering (see canonCompare and ORIENT).
orient :: Type -> Type -> Constraint
orient a b | canonCompare a b == GT = b :~: a
           | otherwise              = a :~: b

-- Combine two canonical constraints into one
interact :: Constraint -> Constraint -> Maybe Constraints
-- EQSAME: Two equalities with the same LHS are combined to equate the RHS.
interact c1@(TVar v1 :~: t1) c2@(TVar v2 :~: t2)
  | v1 == v2 && isCanonical c1 && isCanonical c2 = Just
    [TVar v1 :~: t1, t1 :~: t2]

-- EQDIFF: One equality can be substituted into the other. We rely the ORIENT
-- rule in prior canonicalisation to ensure this makes progress.
interact c1@(TVar v1 :~: t1) c2@(TVar v2 :~: t2)
  | v1 `member` ftv t2 && isCanonical c1 && isCanonical c2 = Just
    [TVar v1 :~: t1, TVar v2 :~: sub (singleton v1 t1) t2]

-- EQAPP (invented): We can substitute an equality into any other equality
interact c1@(TVar v1 :~: t1) (t2 :~: t3)
  | ((v1 `member` ftv t2) || (v1 `member` ftv t3)) && isCanonical c1 = Just
    [c1, sub (singleton v1 t1) t2 :~: sub (singleton v1 t1) t3]

-- EQRECORD (invented): We can substitute an equality into a HasField constraint
interact c1@(TVar v1 :~: t1) (HasField r l t)
  | v1 `member` ftv r && isCanonical c1 = Just
    [TVar v1 :~: t1, HasField (sub (singleton v1 t1) r) l t]
  | v1 `member` ftv t && isCanonical c1 = Just
    [TVar v1 :~: t1, HasField r l (sub (singleton v1 t1) t)]

-- If no rules match, signal failure
interact _ _ = Nothing

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
  = TVar v2 :~: sub (singleton v1 t1) t2

-- If no rules match, return the wanted constraint unchanged
simplify _ w = w

contains :: Type -> Type -> Bool
contains a b | a == b = True
contains (TApp a b) t = any (`contains` t) [a, b]
contains _          _ = False

canonCompare :: Type -> Type -> Ordering
canonCompare (TVar (U _)) (TVar (R _)) = LT
canonCompare (TVar (R _)) (TVar (U _)) = GT
canonCompare (TVar a    ) (TVar b    ) = compare a b
canonCompare (TVar _    ) _            = LT
canonCompare _            (TVar _)     = GT
canonCompare _            (TCon _)     = LT
canonCompare (TCon _)     _            = GT
canonCompare _            (TApp _ _)   = LT
canonCompare (TApp _ _)   _            = GT
canonCompare _            _            = EQ

-- Fig. 20
-- A constraint is canonical if it has the form (v ~ t) and v is not free in t
isCanonical :: Constraint -> Bool
isCanonical (TVar v :~: t) | v `member` ftv t = False
                           | otherwise        = True
isCanonical _ = False

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

-- | Unfold a nested type application
-- f a b c ==> [f, a, b, c]
unfoldTyApp :: Type -> [Type]
unfoldTyApp (TApp a b) = unfoldTyApp a <> [b]
unfoldTyApp t          = [t]
