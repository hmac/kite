module Canonical
  ( module Canonical
  , Syn.Import(..)
  ) where

-- A canonical representation of a module.
-- This means all names are resolved to a specific scope, i.e. local or from a
-- particular named module.

import qualified AST                            ( Expr
                                                , Pat
                                                )
import           Data.Name
import           Syn                     hiding ( Name )

type Exp = AST.Expr Name (Type_ Name)
type Type = Type_ Name
type Pattern = AST.Pat Name
type DataCon = DataCon_ Name
type Data = Data_ Name
type Alias = Alias_ Name
type Fun a = Fun_ Name a (Type_ Name)
type Decl a = Decl_ Name a (Type_ Name)
type ImportItem = ImportItem_ RawName
type Module = Module_ Name Exp (Type_ Name)
