-- The model for typing constraints

module Constraint
  ( Constraint(..)
  , CConstraint(..)
  , Type(..)
  , Var(..)
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
-- this case ensures that <> is associative
  (c :^^: d)  <> e           = c :^^: (d :^^: e)
  c           <> d           = c :^^: d

instance Monoid CConstraint where
  mempty = Simple CNil

data Constraint = CNil
                | Constraint :^: Constraint
                | Type :~: Type
                deriving (Eq, Show, Ord)

data Type = TVar Var
          | TCon RawName [Type]
          deriving (Eq, Show, Ord)

data Var = R RawName
         | U RawName
         deriving (Eq, Show, Ord)
