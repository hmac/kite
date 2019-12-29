module Canonical where

-- A canonical representation of a module.
-- This means all names are resolved to a specific scope, i.e. local or from a
-- particular named module.

import           Data.Name                      ( ModuleName(..)
                                                , RawName(..)
                                                )
import           Syntax                  hiding ( Name )

data Name
  = Local RawName
  | TopLevel ModuleName RawName
  deriving (Eq, Show, Ord)

fromLocal :: Name -> RawName
fromLocal (Local n) = n
fromLocal n         = error $ "Expected Local name, found " <> show n

type Exp = Syn_ Name
type Type = Ty_ Name
type Pattern = Pattern_ Name
type DataCon = DataCon_ Name
type Instance = Instance_ Name
type Typeclass = Typeclass_ Name
type Data = Data_ Name
type Def = Def_ Name
type Fun = Fun_ Name
type Constraint = Constraint_ Name
type Decl = Decl_ Name
type Import = Import_ Name
type Module = Module_ Name
