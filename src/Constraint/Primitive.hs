module Constraint.Primitive
  ( env
  , typeclasses
  )
where

import           Constraint.Generate.M
import           Constraint
import           Constraint.Generate.Module     ( Typeclasses )
import           Canonical                      ( Name(..) )
import           Constraint.Expr

import qualified Data.Map.Strict               as Map

env :: TypeEnv
env = Map.fromList
  -- (::) : a -> [a] -> [a]
  [ ( TopLevel modPrim "::"
    , Forall
      [R "a"]
      mempty
      (TVar (R "a") `fn` TCon (TopLevel modPrim "List") [TVar (R "a")] `fn` TCon
        (TopLevel modPrim "List")
        [TVar (R "a")]
      )
    )
  , (TopLevel modPrim "+", Forall [] mempty (TInt `fn` TInt `fn` TInt))
  , ( TopLevel modPrim "appendString"
    , Forall [] mempty (TString `fn` TString `fn` TString)
    )
  ]

-- Builtin typeclass names
-- Currently none, but we might add some in the future
-- e.g. Semigroup, so we can append strings in string interpolation
typeclasses :: Typeclasses
typeclasses = mempty
