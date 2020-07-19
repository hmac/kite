module Data.Name where

import           Data.List                      ( intersperse )
import           Data.String                    ( IsString(fromString) )
import           Util

-- Shared types of name

newtype RawName = Name String
  deriving (Eq, Ord)

instance Show RawName where
  show (Name s) = s

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

-- TODO: change these RawNames to Text/String?
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

toString :: Name -> String
toString = show . toRaw

-- If the name contains no dots, interpret it as local
-- If it contains dots, interpret it as a top level name qualified with a
-- module.
instance IsString Name where
  fromString s = case splitOn '.' s of
    [n] -> Local (Name n)
    ns  -> TopLevel (ModuleName (init ns)) (Name (last ns))
