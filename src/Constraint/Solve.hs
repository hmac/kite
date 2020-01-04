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
-- Two more cases for interact: EQDICT, DDICT
-- the SIMPLIFY rule (except for SEQFEQ and SFEQFEQ)
-- the TOPREACT rule (except for FINST)

module Constraint.Solve
  ( solve
  , solveDebug
  )
where

import           Data.List                      ( sort )
import           Control.Monad.State.Strict
import           Constraint
import           Prelude                 hiding ( interact )

type Solve = State [Constraint]

-- Solve a set of constraints
-- Repeatedly applies rewrite rules until there's nothing left to do
solve :: [Constraint] -> Either Error [Constraint]
solve cs = case applyRewrite cs of
  Left  err -> Left err
  Right cs' -> if sort cs == sort cs' then Right cs' else solve cs'

-- Like solve but shows the solving history
solveDebug :: [Constraint] -> Either Error [[Constraint]]
solveDebug cs = go cs [cs]
 where
  go d hist = case applyRewrite d of
    Left err -> Left err
    Right d' ->
      if sort d == sort d' then Right (d' : hist) else go d' (d' : hist)

run :: Solve (Either Error ()) -> [Constraint] -> Either Error [Constraint]
run f c = case runState f c of
  (Right (), c') -> Right c'
  (Left  e , _ ) -> Left e

-- Apply a round of rewriting
applyRewrite :: [Constraint] -> Either Error [Constraint]
applyRewrite c = case run canonM c of
  Left  e  -> Left e
  Right c' -> run interactM c'

interactM :: Solve (Either Error ())
interactM = do
  constraints <- get
  case firstJust (map interactEach (focusPairs constraints)) of
    Just constraints' -> do
      put constraints'
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

canonM :: Solve (Either Error ())
canonM = do
  constraints <- get
  case canonAll constraints of
    Left  err          -> pure $ Left err
    Right constraints' -> do
      put constraints'
      pure $ Right ()
 where
  canonAll :: [Constraint] -> Either Error [Constraint]
  canonAll []       = Right []
  canonAll (c : cs) = case canon c of
    Left  err -> Left err
    Right c'  -> (flatten c' ++) <$> canonAll cs

flatten :: Constraint -> [Constraint]
flatten (a :^: b) = a : flatten b
flatten c         = [c]

data Error = OccursCheckFailure
           | ConstructorMismatch
  deriving (Show, Eq)

-- Canonicalise a constraint
canon :: Constraint -> Either Error Constraint
-- REFL: Reflexive equalities can be ignored
canon (a :~: b) | a == b = pure CNil

-- TDEC: Equalities between identical constructors can be decomposed to
-- equalities on their arguments
canon (TCon k as :~: TCon k' bs) | k == k' =
  pure $ foldl (:^:) CNil (zipWith (:~:) as bs)

-- FAILDEC: Equalities between constructor types must have the same constructor
canon (TCon k _ :~: TCon k' _) | k /= k'            = Left ConstructorMismatch

-- OCCCHECK: a type variable cannot be equal to a type containing that variable
canon (v@(TVar _) :~: t) | v /= t && t `contains` v = Left OccursCheckFailure

-- ORIENT: Flip an equality around if desirable
canon (a :~: b) | canonCompare a b == GT            = pure (b :~: a)

-- Custom rule: CNil ^ c = c
canon (CNil :^: c   )                               = pure c
canon (c    :^: CNil)                               = pure c

-- Flattening rules only apply to type classes and type families, so are
-- omitted.
canon c                                             = pure c

-- Combine two canonical constraints into one
interact :: Constraint -> Constraint -> Maybe Constraint
-- EQSAME: Two equalities with the same LHS are combined to equate the RHS.
interact (TVar a :~: b) (TVar a' :~: c) | a == a' =
  Just $ (TVar a :~: b) :^: (b :~: c)

-- EQDIFF: One equality can be substituted into the other. We rely the ORIENT
-- rule in on prior canonicalisation to ensure this makes progress.
interact (TVar v1 :~: t1) (TVar v2 :~: t2) | v1 `elem` ftv t2 =
  Just $ (TVar v1 :~: t1) :^: (TVar v2 :~: subst v1 t1 t2)

-- Redundant cases: drop CNil
interact CNil c    = Just c
interact c    CNil = Just c

-- If no rules match, signal failure
interact _    _    = Nothing

-- The simplify rules are omitted because I don't think they're relevant without
-- typeclasses and type families. May need to revisit this if I'm wrong.

-- The topreact rules are omitted because they're not relevant without
-- typeclasses.

contains :: Type -> Type -> Bool
contains a b | a == b  = True
contains (TCon _ ts) t = any (`contains` t) ts
contains _           _ = False

canonCompare :: Type -> Type -> Ordering
canonCompare (TVar (U _)) (TVar (R _)) = LT
canonCompare (TVar (R _)) (TVar (U _)) = GT
canonCompare (TVar a    ) (TVar b    ) = compare a b
canonCompare _            (TCon _ _  ) = LT
canonCompare (TCon _ _)   _            = GT

-- Calculate the free type variables of a type
ftv :: Type -> [Var]
ftv (TVar v   ) = [v]
ftv (TCon _ ts) = concatMap ftv ts

-- Substitute a variable for a type in a type
subst :: Var -> Type -> Type -> Type
subst var sub (TVar v) | v == var = sub
subst var sub (TCon c ts)         = TCon c (map (subst var sub) ts)
subst _   _   t                   = t

-- A list of each element in the given list paired with the remaining elements
focus :: [a] -> [(a, [a])]
focus = go []
 where
  go ys []       = []
  go ys (x : xs) = (x, reverse ys ++ xs) : go (x : ys) xs

focusPairs :: [a] -> [(a, a, [a])]
focusPairs xs =
  concatMap (\(y, ys) -> map (\(z, zs) -> (y, z, zs)) (focus ys)) (focus xs)

-- Extract the first Just value from a list
firstJust :: [Maybe a] -> Maybe a
firstJust []             = Nothing
firstJust (Just x  : xs) = Just x
firstJust (Nothing : xs) = firstJust xs
