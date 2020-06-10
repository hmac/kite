module Constraint.Primitive
  ( env
  , io
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
      (    TVar (R "a")
      `fn` TApp (TCon (TopLevel modPrim "List")) (TVar (R "a"))
      `fn` TApp (TCon (TopLevel modPrim "List")) (TVar (R "a"))
      )
    )
  , (TopLevel modPrim "+", Forall [] (TInt `fn` TInt `fn` TInt))
  , (TopLevel modPrim "-", Forall [] (TInt `fn` TInt `fn` TInt))
  , (TopLevel modPrim "*", Forall [] (TInt `fn` TInt `fn` TInt))
  , ( TopLevel modPrim "appendString"
    , Forall [] (TString `fn` TString `fn` TString)
    )
  , (TopLevel modPrim "$showInt", Forall [] (TInt `fn` TString))
  , (TopLevel modPrim "$eqInt"  , Forall [] (TInt `fn` TInt `fn` TBool))
  ]

io :: Type
io = TCon (TopLevel modPrim "IO")
