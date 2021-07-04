{-# LANGUAGE DeriveGeneric #-}
module Package
  ( loadAndBuildPackageInfo
  , PackageInfo(..)
  , Error(..)
  ) where

import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty
                                                , pretty
                                                )
import           GHC.Generics                   ( Generic )
import           Package.Spec                   ( DepSpec(..)
                                                , Spec(..)
                                                , parseSpec
                                                )
import           System.Directory               ( doesFileExist
                                                , getCurrentDirectory
                                                )

import           Data.Name                      ( PackageName
                                                , mkPackageName
                                                )


-- Kite packages are named collections of modules.

-- Packages can either be in a package registry or locally on-disk

-- In an application we write a kite-package.toml file with something like
--
--     [dependencies]
--     foo = { type = "local", path = "./foo" }
--     bar = { type = "remote", version = "1.2.3" }
--
-- Kite will then look for the package 'foo' in $PWD/foo.
--
-- Kite will look for 'bar' in $XDG_CACHE_HOME/kite/bar/1.2.3. If it isn't there then it will fetch
-- bar from the package registry and store it there. It should also do some sort of checksum
-- comparison to make sure that the contents of bar are as expected.
--
-- When loading a source file, Kite will look up the source files of the imports by mapping the
-- package name to its path. If no package name is given, the import is assumed to be local and the
-- path will be relative to the application root directory.
--
--
-- So the process for Kite to run should be:
-- 1. Find and read the kite-package.toml file. It should be in the current directory.
--    Stretch goal: look in parent directories too.
-- 2. Use the list of packages in the toml file to map each package to a directory path.
--    Fetch any remote packages from the package repository and save them in the cache directory.
-- 3. Start loading all Kite source files in the application source directory.
--    When an import is encountered, load the corresponding file, using the package info collected
--    earlier to determine the file path.
-- 4. Namespace the imported module by its package so it doesn't clash with identically named
--    modules in other packages.
-- 5. Carry on as usual.

data Error = NoKitePackageToml
           | KitePackageTomlParseError String
           | InvalidPackageName String
           | RemoteDepsNotSupported
  deriving (Eq, Generic, Show)

instance Pretty Error where
  pretty = \case
    NoKitePackageToml              -> "No kite-package.toml found"
    KitePackageTomlParseError e    -> pretty e
    InvalidPackageName        name -> "Invalid package name:" <+> pretty name
    RemoteDepsNotSupported -> "Remote dependencies are not yet supported"

-- | A combination of 'loadPackageConfigFile' followed by 'buildPackageInfo'
loadAndBuildPackageInfo :: (MonadError Error m, MonadIO m) => m PackageInfo
loadAndBuildPackageInfo = do
  spec <- loadPackageConfigFile
  buildPackageInfo spec

loadPackageConfigFile :: (MonadError Error m, MonadIO m) => m Spec
loadPackageConfigFile = do
  -- We assume that the file is in the current working directory
  dir <- liftIO getCurrentDirectory
  let path = dir <> "/kite-package.toml"
  kitePackageTomlExists <- liftIO $ doesFileExist path
  if kitePackageTomlExists
    then do
      file <- liftIO $ readFile path
      case parseSpec (pack file) of
        Left  err  -> throwError $ KitePackageTomlParseError $ unpack err
        Right spec -> pure spec
    else throwError NoKitePackageToml

-- Convert a 'PackageSpec' to a 'PackageInfo'.
-- For now this is a dumb translation, but in the future we will need to e.g. fetch package contents
-- from the package repository in order to do determine the path for each dependency.
buildPackageInfo :: (MonadError Error m, MonadIO m) => Spec -> m PackageInfo
buildPackageInfo spec = do
  dir  <- liftIO getCurrentDirectory
  deps <- mapM validateDep (specDependencies spec)
  name <- case mkPackageName (specName spec) of
    Just n  -> pure n
    Nothing -> throwError $ InvalidPackageName $ show $ specName spec
  pure PackageInfo { packageRootDir = dir
                   , packageDeps    = Map.fromList deps
                   , packageName    = name
                   }
 where
  -- We currently only support local packages, and the package name must obey our rules for package
  -- names.
  validateDep LocalDep { depSpecName = n, depSpecPath = p } =
    case mkPackageName n of
      Just pname -> pure (pname, p)
      Nothing    -> throwError $ InvalidPackageName n
  validateDep _ = throwError RemoteDepsNotSupported

-- Information about the package we are building.
-- For now, this just contains a map from dependency package names to their path on disk.
data PackageInfo = PackageInfo
  -- A map from package names to the directory where their code is stored.
  { packageDeps    :: Map PackageName FilePath
  -- The root directory we're working from
  , packageRootDir :: FilePath
  , packageName    :: PackageName
  }
  deriving (Eq, Show, Generic)
