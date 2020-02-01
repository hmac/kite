module Canonical where

-- A canonical representation of a module.
-- This means all names are resolved to a specific scope, i.e. local or from a
-- particular named module.

import           Data.String                    ( IsString(fromString) )
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

instance IsString Name where
  fromString = Local . Name

type Exp = Syn_ Name
type Type = Type_ Name
type Pattern = Pattern_ Name
type DataCon = DataCon_ Name
type Instance = Instance_ Name
type Typeclass = Typeclass_ Name
type Data = Data_ Name
type Def = Def_ Name
type Fun a = Fun_ Name a (Type_ Name)
type Constraint = Constraint_ Name
type Decl a = Decl_ Name a (Type_ Name)
type Import = Import_ Name
type Module a = Module_ Name a (Type_ Name)
