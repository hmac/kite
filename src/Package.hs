module Package where

import           Package.Spec                   ( PkgSpec(..)
                                                , DepSpec(..)
                                                , DepSource(..)
                                                , VersionSpec(..)
                                                , VersionNum(..)
                                                , PkgName
                                                )

loadCurrentLocalPackage :: FilePath -> IO ()
loadCurrentLocalPackage searchPath = do
  -- Walk up path until we find a kite-package.dhall
  -- Load that file and parse it to a PkgSpec
  -- Load all dependent packages (recursively)
  -- Solve constraints for each package to a specific version
  -- This gives a set of packages and their precise versions
  -- Fetch each package from its source
  -- For each package:
  --   Load all modules into a 'realised' package or whatever (a module group)
  -- Typecheck the root package, with the others in the environment
  -- Done.
  undefined searchPath

loadPackage :: FilePath -> DepSpec -> IO ()
loadPackage root depSpec = do
  -- If source is local:
  --   Construct abs path using root
  --   Find kite-package.dhall
  --   Load and parse it
  -- If package is remote:
  --   Hit URL for package info
  --   Load and parse it
  --
  -- Load all dependent packages
  undefined root depSpec

loadRemotePackage :: PkgName -> VersionSpec -> String -> IO ()
loadRemotePackage pkgName versionSpec remoteUrl = do
  undefined pkgName versionSpec remoteUrl
  -- Hit the URL to fetch the package info
  -- Parse the spec and return it
  -- 
  -- How do we know what version to request?
  -- What are we returning?
