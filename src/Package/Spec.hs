{-# LANGUAGE DeriveGeneric #-}
module Package.Spec
  ( parseSpec
  , printSpec
  , Spec(..)
  , DepSpec(..)
  , Version(..)
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Text                      ( Text )
import           GHC.Generics
import           Toml                           ( (.=)
                                                , TomlCodec
                                                )
import qualified Toml

import           Util

-- | The kite-package.toml definition
data Spec = Spec
  { specDependencies :: [DepSpec]
  , specName         :: String
  }
  deriving (Eq, Show, Generic)

data DepSpec =
    LocalDep { depSpecName :: String, depSpecPath :: FilePath }
  | RemoteDep { depSpecName :: String, depSpecVersion :: Version }
  deriving (Eq, Show, Generic)

data Version = Version
  { versionMajor :: Word
  , versionMinor :: Word
  , versionPatch :: Word
  }
  deriving (Eq, Show, Generic)

parseSpec :: Text -> Either Text Spec
parseSpec = first Toml.prettyTomlDecodeErrors . Toml.decode specCodec

printSpec :: Spec -> Text
printSpec = Toml.encode specCodec

specCodec :: TomlCodec Spec
specCodec =
  Spec
    <$> Toml.list depCodec "dependencies"
    .=  specDependencies
    <*> Toml.string "name"
    .=  specName

depCodec :: TomlCodec DepSpec
depCodec =
  Toml.dimatch matchLocalDep id localDepCodec
    <|> Toml.dimatch matchRemoteDep id remoteDepCodec
 where
  matchLocalDep d@LocalDep{} = Just d
  matchLocalDep _            = Nothing
  matchRemoteDep d@RemoteDep{} = Just d
  matchRemoteDep _             = Nothing

localDepCodec :: TomlCodec DepSpec
localDepCodec =
  LocalDep
    <$> Toml.string "name"
    .=  depSpecName
    <*> Toml.string "path"
    .=  depSpecPath

remoteDepCodec :: TomlCodec DepSpec
remoteDepCodec =
  RemoteDep
    <$> Toml.string "name"
    .=  depSpecName
    <*> Toml.table versionCodec "version"
    .=  depSpecVersion

versionCodec :: TomlCodec Version
versionCodec =
  Version
    <$> Toml.word "major"
    .=  versionMajor
    <*> Toml.word "minor"
    .=  versionMinor
    <*> Toml.word "patch"
    .=  versionPatch
