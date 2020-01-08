-- The model for typing constraints

module Constraint
  ( Constraint(..)
  , CConstraint(..)
  , Type(..)
  , Var(..)
  )
where

import           Data.Name

data CConstraint = Simple Constraint
                | CConstraint :^^: CConstraint
                -- these are always unification vars
                | E [Var] Constraint CConstraint
                deriving (Eq, Show, Ord)

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
