module Data.Name where

import           Data.String                    ( IsString(fromString) )
import           Util

-- Shared types of name

newtype RawName = Name String
  deriving (Eq, Show, Ord)

instance IsString RawName where
  fromString = Name

newtype ModuleName = ModuleName [String]
  deriving (Eq, Show, Ord)

instance IsString ModuleName where
  fromString s = ModuleName $ splitOn '.' s
