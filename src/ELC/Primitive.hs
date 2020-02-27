module ELC.Primitive where

-- Definitions for primitive types, constructors and functions

import           Data.Name
import           Canonical
import           ELC
import qualified Syn.Typed                     as T

modPrim :: ModuleName
modPrim = ModuleName ["Lam", "Primitive"]

-- Constructors

listNil :: Con
listNil = Sum { conName   = TopLevel modPrim "[]"
              , conArity  = 0
              , sumTag    = 0
              , sumFamily = [listNil, listCons]
              }
listCons :: Con
listCons = Sum { conName   = TopLevel modPrim "::"
               , conArity  = 2
               , sumTag    = 1
               , sumFamily = [listNil, listCons]
               }
tuple2 :: Con
tuple2 = Prod { conName = TopLevel modPrim "(,)", conArity = 2 }
tuple3 :: Con
tuple3 = Prod { conName = TopLevel modPrim "(,,)", conArity = 3 }
tuple4 :: Con
tuple4 = Prod { conName = TopLevel modPrim "(,,,)", conArity = 4 }
tuple5 :: Con
tuple5 = Prod { conName = TopLevel modPrim "(,,,,)", conArity = 5 }
tuple6 :: Con
tuple6 = Prod { conName = TopLevel modPrim "(,,,,,)", conArity = 6 }

primConstructors :: [(Name, ELC.Exp)]
primConstructors = map
  extract
  [listNil, listCons, tuple2, tuple3, tuple4, tuple5, tuple6]
  where extract con = (conName con, Cons con [])

primInstances :: [((Name, T.Type), Name)]
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
