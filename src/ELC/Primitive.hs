module ELC.Primitive where

-- Definitions for primitive types, constructors and functions

import           Data.Name
import           Canonical
import           ELC

modPrim :: ModuleName
modPrim = ModuleName ["Lam", "Primitive"]

-- Constructors

listNil :: Con
listNil = Sum { name   = TopLevel modPrim "[]"
              , tag    = 0
              , arity  = 0
              , family = [listNil, listCons]
              }
listCons :: Con
listCons = Sum { name   = TopLevel modPrim "::"
               , tag    = 1
               , arity  = 2
               , family = [listNil, listCons]
               }
tuple2 :: Con
tuple2 = Prod { name = TopLevel modPrim "(,)", arity = 2 }
tuple3 :: Con
tuple3 = Prod { name = TopLevel modPrim "(,,)", arity = 3 }
tuple4 :: Con
tuple4 = Prod { name = TopLevel modPrim "(,,,)", arity = 4 }
tuple5 :: Con
tuple5 = Prod { name = TopLevel modPrim "(,,,,)", arity = 5 }
tuple6 :: Con
tuple6 = Prod { name = TopLevel modPrim "(,,,,,)", arity = 6 }

primConstructors :: [(Name, ELC.Exp)]
primConstructors = map
  extract
  [listNil, listCons, tuple2, tuple3, tuple4, tuple5, tuple6]
  where extract con = (name con, Cons con [])

primInstances :: [((Name, [Type]), Name)]
primInstances = []

-- Functions

add :: Name
add = TopLevel modPrim "+"

mult :: Name
mult = TopLevel modPrim "*"

sub :: Name
sub = TopLevel modPrim "-"

primShow :: Name
primShow = TopLevel modPrim "show"

primAppendString :: Name
primAppendString = TopLevel modPrim "appendString"
