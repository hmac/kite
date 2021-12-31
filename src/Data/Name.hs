module Data.Name
  ( RawName(..)
  , ModuleName(..)
  , PkgModuleName(..)
  , showModuleName
  , PackageName(..)
  , mkPackageName
  , Name(..)
  , fromLocal
  , toRaw
  , toString
  , localise
  , prim
  ) where

import           Data.Char                      ( isAsciiLower )
import           Data.Data                      ( Data )
import           Data.List                      ( intersperse )
import           Data.String                    ( IsString(fromString) )
import           Data.Text.Prettyprint.Doc      ( Pretty
                                                , hcat
                                                , pretty
                                                , punctuate
                                                )
import           GHC.Generics                   ( Generic )
import           Type.Reflection                ( Typeable )
import           Util

-- Shared types of name

newtype RawName = Name String
  deriving (Eq, Ord, Typeable, Data, Generic)

instance Show RawName where
  show (Name s) = s

instance IsString RawName where
  fromString = Name

newtype ModuleName = ModuleName [String]
  deriving (Eq, Ord, Typeable, Data, Generic)

instance Show ModuleName where
  show = showModuleName

showModuleName :: ModuleName -> String
showModuleName (ModuleName names) = mconcat (intersperse "." names)

instance IsString ModuleName where
  fromString s = ModuleName $ splitOn '.' s

instance Pretty ModuleName where
  pretty (ModuleName names) = hcat $ punctuate "." $ map pretty names

newtype PackageName = PackageName String
  deriving (Eq, Ord, Typeable, Data, Generic)

instance Show PackageName where
  show (PackageName n) = n

instance Pretty PackageName where
  pretty (PackageName n) = pretty n

instance IsString PackageName where
  fromString s = case mkPackageName s of
    Just p  -> p
    Nothing -> error $ "Invalid package name: " <> show s

-- A valid package name consists of characters in the regex range [a-z-]
mkPackageName :: String -> Maybe PackageName
mkPackageName s
  | all (\c -> isAsciiLower c || c == '-') s = Just $ PackageName s
  | otherwise = Nothing

data PkgModuleName = PkgModuleName PackageName ModuleName
  deriving (Eq, Ord, Typeable, Data, Generic)

instance Show PkgModuleName where
  show (PkgModuleName pkgName modName) = show pkgName <> "." <> show modName

instance Pretty PkgModuleName where
  pretty (PkgModuleName pkgName modName) =
    pretty pkgName <> "." <> pretty modName

instance IsString PkgModuleName where
  fromString (c : s) | isAsciiLower c = case splitOn '.' (c : s) of
    pkg : ms -> PkgModuleName (PackageName pkg) (ModuleName ms)
    _ -> error $ "PkgModuleName: invalid package/module name: " <> show (c : s)
  fromString s =
    error $ "PkgModuleName: invalid package/module name: " <> show s

-- TODO: change these RawNames to Text/String?
data Name
  = Local RawName
  | TopLevel PkgModuleName RawName
  deriving (Eq, Ord, Typeable, Data, Generic)

instance Show Name where
  show (Local (Name name)) = "\"" <> name <> "\""
  show (TopLevel moduleName (Name name)) =
    "\"" <> show moduleName <> "." <> name <> "\""

instance Pretty Name where
  pretty (Local (Name n)) = pretty n
  pretty (TopLevel pkgModName (Name n)) =
    hcat $ punctuate "." [pretty (show pkgModName), pretty n]

fromLocal :: Name -> RawName
fromLocal (Local n) = n
fromLocal n         = error $ "Expected Local name, found " <> show n

toRaw :: Name -> RawName
toRaw (Local n     ) = n
toRaw (TopLevel _ n) = n

toString :: Name -> String
toString (Local n     ) = show n
toString (TopLevel m n) = show m <> "." <> show n

-- If the name belongs to the module given, drop the module prefix.
localise :: PkgModuleName -> Name -> Name
localise modName (TopLevel modName' n) | modName == modName' = Local n
localise _ n = n

-- If the name contains no dots, interpret it as local
-- If it contains dots, interpret it as a top level name qualified with a
-- module.
instance IsString Name where
  fromString s = case splitOn '.' s of
    [n] -> Local (Name n)
    ns  -> case init ns of
      (p : pkg) : mods | isAsciiLower p -> TopLevel
        (PkgModuleName (PackageName (p : pkg)) (ModuleName mods))
        (Name (last ns))
      _ -> error $ "Invalid Name: " <> show s

instance Debug Name where
  debug = toString

prim :: RawName -> Name
prim = TopLevel (PkgModuleName "kite" (ModuleName ["Kite", "Prim"]))
