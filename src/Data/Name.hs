module Data.Name where

import           Data.String                    ( IsString(fromString) )
import           Util

-- Shared types of name

-- TODO: move Canonical.Name to here
-- TODO: Rename RawName.Name constructor to Name, and prefer to construct literal
--       elements of this type using the IsString instance.

newtype RawName = Name String
  deriving (Eq, Show, Ord)

instance IsString RawName where
  fromString = Name

newtype ModuleName = ModuleName [String]
  deriving (Eq, Show, Ord)

instance IsString ModuleName where
  fromString s = ModuleName $ splitOn '.' s
