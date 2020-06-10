module LC where

-- Simple lambda calculus

import           Data.Map.Strict                ( Map )
import           Data.Name                      ( Name(..) )
import           ELC                            ( Constant(..)
                                                , Con(..)
                                                )


data Exp = Const Constant [Exp]
         | Var Name
         | Cons Con [Exp]
         | App Exp Exp
         | Abs Name Exp
         -- We include simple lets here because it is more efficient to execute
         -- them directly than to convert them to abstractions.
         | Let Name Exp Exp
         | Bottom String
         | Fail
         | Fatbar Exp Exp
         | If Exp Exp Exp
         | Eq Exp Exp
         -- UnpackProduct i f a  expects a to be a product of arity i, unpacks
         -- it, and passes the values inside it as arguments to f
         | UnpackProduct Int Exp Exp
         -- UnpackSum t i f a  expects a to be a sum with tag t, arity i
         -- it unpacks it and passes the values inside it as arguments to f
         | UnpackSum Int Int Exp Exp
         | Project Int Int Exp   -- arity of the constructor; index of the field
         -- The Y combinator
         | Y Exp
         -- CASE-N inspects the tag of the expression (expected to be a sum
         -- constructor) and selects the corresponding branch.
         -- CASE-N n (si ...) b1...bn = bi
         | CaseN Exp [Exp]
         | Record (Map String Exp)
         | RecordProject Exp String
         | FCall String [Exp]
         deriving (Eq, Show)

type Env = [(Name, Exp)]
