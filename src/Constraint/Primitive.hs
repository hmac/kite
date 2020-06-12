module Constraint.Primitive
  ( env
  , io
  , fcallInfo
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

-- In the future we will support syntax to declare the types of foreign calls,
-- like this:
--     foreign putStrLn : String -> IO ()
--     foreign getLine : IO String
--     foreign bindIO : IO a -> (a -> IO b) -> IO b
-- Until then, we hard-code the types here.
-- If you add a new fcall here, you also need to add its implementation in
-- LC.Execute.
type FCallInfo = ([Var], [Type], Type)

fcallInfo :: [(String, FCallInfo)]
fcallInfo =
  [ ("putStrLn", ([], [TString], TApp io TUnit))
  , ("putStr"  , ([], [TString], TApp io TUnit))
  , ("getLine" , ([], [], TApp io TString))
  , ("pureIO"  , ([R "a"], [TVar (R "a")], TApp io (TVar (R "a"))))
  , ( "bindIO"
    , ( [R "a", R "b"]
      , [TApp io (TVar (R "a")), TVar (R "a") `fn` TApp io (TVar (R "b"))]
      , TApp io (TVar (R "b"))
      )
    )
  ]
