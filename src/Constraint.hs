-- The model for typing constraints

module Constraint
  ( Constraint(..)
  , CConstraint(..)
  , Type(..)
  , Var(..)
  , Subst
  , simple
  , implic
  , fn
  )
where

import           Data.Name

-- Consider making constraints a monoid with <> = :^^: and mempty = Simple CNil
data CConstraint = Simple Constraint
                | CConstraint :^^: CConstraint
                -- these are always unification vars
                | E [Var] Constraint CConstraint
                deriving (Eq, Show, Ord)

instance Semigroup CConstraint where
  Simple CNil <> c           = c
  c           <> Simple CNil = c
-- Note: this case ensures that <> is associative, but may be very inefficient
  (c :^^: d)  <> e           = c <> (d <> e)
  c           <> d           = c :^^: d

instance Monoid CConstraint where
  mempty = Simple CNil

data Constraint = CNil
                | Constraint :^: Constraint
                | Type :~: Type
                deriving (Eq, Show, Ord)

instance Semigroup Constraint where
  CNil      <> c    = c
  c         <> CNil = c
-- Note: this case ensures that <> is associative, but may be very inefficient
  (c :^: d) <> e    = c <> (d <> e)
  c         <> d    = c :^: d

instance Monoid Constraint where
  mempty = CNil

data Type = TVar Var
          | TCon RawName [Type]
          deriving (Eq, Show, Ord)

data Var = R RawName
         | U RawName
         deriving (Eq, Show, Ord)

-- The type of substitutions
type Subst a b = [(a, b)]

fn :: Type -> Type -> Type
a `fn` b = TCon "->" [a, b]

simple :: CConstraint -> [Constraint]
simple E{}        = mempty
simple (Simple c) = [c]
simple (c :^^: d) = simple c <> simple d

implic :: CConstraint -> CConstraint
implic (Simple _) = mempty
implic e@E{}      = e
implic (c :^^: d) = implic c <> implic d
