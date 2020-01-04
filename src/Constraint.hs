-- The model for typing constraints

module Constraint
  ( Constraint(..)
  , Type(..)
  , Var(..)
  )
where

data Constraint = CNil
                | Constraint :^: Constraint
                | Type :~: Type
                deriving (Eq, Show, Ord)

data Type = TVar Var
          | TCon String [Type]
          deriving (Eq, Show, Ord)

data Var = R String
         | U String
         deriving (Eq, Show, Ord)
