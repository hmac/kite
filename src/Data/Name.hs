module Data.Name where

import           Data.List                      ( intersperse )
import           Data.String                    ( IsString(fromString) )
import           Util

-- Shared types of name

-- TODO: Rename RawName.Name constructor to Name, and prefer to construct literal
--       elements of this type using the IsString instance.

newtype RawName = Name String
  deriving (Eq, Show, Ord)

instance IsString RawName where
  fromString = Name

newtype ModuleName = ModuleName [String]
  deriving (Eq, Ord)

instance Show ModuleName where
  show = showModuleName

showModuleName :: ModuleName -> String
showModuleName (ModuleName names) = mconcat (intersperse "." names)

instance IsString ModuleName where
  fromString s = ModuleName $ splitOn '.' s

data Name
  = Local RawName
  | TopLevel ModuleName RawName
  deriving (Eq, Ord)

instance Show Name where
  show (Local (Name name)              ) = "Local " ++ name
  show (TopLevel moduleName (Name name)) = show moduleName ++ "." ++ name

fromLocal :: Name -> RawName
fromLocal (Local n) = n
fromLocal n         = error $ "Expected Local name, found " <> show n

toRaw :: Name -> RawName
toRaw (Local n     ) = n
toRaw (TopLevel _ n) = n

instance IsString Name where
  fromString = Local . Name
