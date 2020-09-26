{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Package.Spec where

import           GHC.Generics
import           Dhall

newtype PkgName = PkgName String
  deriving (Eq, Generic, Show)

instance FromDhall PkgName

data PkgSpec = PkgSpec { name :: PkgName, dependencies :: [DepSpec] }
  deriving (Eq, Generic, Show)

instance FromDhall PkgSpec

data DepSpec = DepSpec { name :: PkgName, version :: VersionSpec, source :: DepSource }
  deriving (Eq, Generic, Show)

instance FromDhall DepSpec

data DepSource = Remote { url :: String }
               | Local { path :: String }
  deriving (Eq, Generic, Show)

instance FromDhall DepSource

data VersionSpec = Equal VersionNum
                 | AtLeast VersionNum
  deriving (Eq, Generic, Show)

instance FromDhall VersionSpec

data VersionNum = VersionNum { major :: Natural, minor :: Natural, patch :: Natural }
  deriving (Eq, Generic, Show)

instance FromDhall VersionNum

pkgName :: Decoder PkgName
pkgName = auto

pkgSpec :: Decoder PkgSpec
pkgSpec = record $ PkgSpec <$> field "name" pkgName <*> field "dependencies"
                                                              (list depSpec)
depSpec :: Decoder DepSpec
depSpec =
  record
    $   DepSpec
    <$> field "name"    pkgName
    <*> field "version" versionSpec
    <*> field "source"  depSource

depSource :: Decoder DepSource
depSource = union
  (  (Remote <$> constructor "remote" remote)
  <> (Local <$> constructor "local" local)
  )
 where
  remote = record $ field "url" string
  local  = record $ field "path" string

versionSpec :: Decoder VersionSpec
versionSpec =
  union
    $  (Equal <$> constructor "eq" versionNum)
    <> (AtLeast <$> constructor "min" versionNum)

versionNum :: Decoder VersionNum
versionNum =
  record
    $   VersionNum
    <$> field "major" natural
    <*> field "minor" natural
    <*> field "patch" natural

load :: Text -> IO PkgSpec
load = input pkgSpec
