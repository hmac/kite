module Constraint.Primitive
  ( env
  )
where

import           Constraint.Generate.M
import           Constraint
import           Data.Name                      ( Name(..) )

import qualified Data.Map.Strict               as Map

env :: TypeEnv
env = Map.fromList
  -- (::) : a -> [a] -> [a]
  [ ( TopLevel modPrim "::"
    , Forall
      [R "a"]
      mempty
      (    TVar (R "a")
      `fn` TApp (TCon (TopLevel modPrim "List")) (TVar (R "a"))
      `fn` TApp (TCon (TopLevel modPrim "List")) (TVar (R "a"))
      )
    )
  , (TopLevel modPrim "+", Forall [] mempty (TInt `fn` TInt `fn` TInt))
  , ( TopLevel modPrim "appendString"
    , Forall [] mempty (TString `fn` TString `fn` TString)
    )
  , (TopLevel modPrim "$showInt", Forall [] mempty (TInt `fn` TString))
  ]
