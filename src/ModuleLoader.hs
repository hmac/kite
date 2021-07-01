module ModuleLoader
  ( ModuleGroup(..)
  , Error(..)
  , loadFromPathAndRootDirectory
  , loadFromPackageInfo
  , readFile'
  ) where

import           Control.Monad.Except           ( MonadError
                                                , runExceptT
                                                , throwError
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.IORef                     ( IORef
                                                , atomicModifyIORef'
                                                , newIORef
                                                , readIORef
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty
                                                , pretty
                                                )
import           GHC.Generics                   ( Generic )
import           System.Directory               ( doesFileExist )

import           AST                            ( Expr )
import           Canonical.Primitive            ( modPrim )
import           Canonicalise                   ( canonicaliseModule )
import           Data.Graph                     ( SCC(..)
                                                , flattenSCCs
                                                , stronglyConnCompR
                                                )
import           Data.List                      ( intercalate )
import           Data.Name                      ( PkgModuleName(..) )
import           ExpandExports                  ( expandExports )
import           ExpandImports                  ( expandImports )
import qualified ExpandImports
import           ModuleGroup
import           Package                        ( PackageInfo(..) )
import           Syn
import           Syn.Parse                      ( parseKiteFile )
import           Util

-- This module is responsible for loading Kite modules. It should attempt to
-- cache modules so they're not loaded multiple times.

-- A typical Kite module might look like this:
--
-- module Foo where
-- import Data.Map
-- fn : Map a b -> Map a b -> Map a b
-- fn = union
--
-- Where Map and union come from Data.Map. When we load dependencies for a
-- module, we want to make all the definitions in Data.Map visible in Foo.
-- To prevent name clashes we qualify all variables with their module.
-- Similarly, if Data.Map depends on any modules we need to do the same for it.
-- For now, we ignore things like aliases, qualification and import lists.

data Error = FileNotFound FilePath
           | ParseError String
           | UnknownPackage PackageName
           | ModuleImportCycle
           | ImportError ExpandImports.Error
  deriving (Eq, Generic, Show)

instance Pretty Error where
  pretty = \case
    FileNotFound   path    -> "File not found:" <+> pretty path
    ParseError     e       -> "Parse error:" <+> pretty e
    UnknownPackage pkgName -> "Unknown package" <+> pretty pkgName
    ModuleImportCycle      -> "Cycle detected in module imports"
    ImportError e          -> case e of
      ExpandImports.CannotFindModule importingModule importedModule ->
        pretty importingModule
          <+> "imports"
          <+> pretty importedModule
          <+> "but this module cannot be found"

-- We use a global cache of parsed modules to prevent parsing the same file twice.
type ModuleCache = IORef (Map String Module)

-- -- TODO: structured errors
-- loadFromPath
--   :: FilePath -> PackageName -> IO (Either String UntypedModuleGroup)
-- loadFromPath path pkgName = do
--   root <- getCurrentDirectory
--   loadFromPathAndRootDirectory path root pkgName

-- Like 'readFile', but checks to see if the file exists before reading it
-- If it doesn't exist, throws a 'FileNotFound' error.
readFile' :: (MonadError Error m, MonadIO m) => FilePath -> m String
readFile' path = do
  exists <- liftIO $ doesFileExist path
  if exists then liftIO (readFile path) else throwError $ FileNotFound path

loadFromPackageInfo
  :: (MonadError Error m, MonadIO m)
  => PackageInfo
  -> FilePath
  -> m UntypedModuleGroup
loadFromPackageInfo info path = do
  fileContents <- readFile' path
  let modul = parseKiteFile path (packageName info) fileContents

  case modul of
    Left  err -> throwError $ ParseError err
    Right m   -> do
      -- Create a mutable map to store the contents of each module as we parse them.
      -- This stops us parsing the same module multiple times.
      cache                          <- liftIO $ newIORef (Map.singleton path m)

      deps <- mapM (loadAll info cache) (dependencies m)
      sortedDeps                     <- sortModules (nub (concat deps))
      (expandedModule, expandedDeps) <-
        runExceptT (expandImports (expandExports m) sortedDeps) >>= \case
          Left  err -> throwError $ ImportError err
          Right r   -> pure r
      pure $ ModuleGroup (canonicaliseModule expandedModule)
                         (map canonicaliseModule expandedDeps)

-- TODO: deprecate and remove in favour of 'loadFromPackageInfo'
loadFromPathAndRootDirectory
  :: (MonadError Error m, MonadIO m)
  => FilePath
  -> FilePath
  -> PackageName
  -> m UntypedModuleGroup
loadFromPathAndRootDirectory path root pkgName = do
  let info = PackageInfo { packageDeps    = mempty
                         , packageRootDir = root
                         , packageName    = pkgName
                         }
  loadFromPackageInfo info path

loadAll
  :: (MonadError Error m, MonadIO m)
  => PackageInfo
  -> ModuleCache
  -> PkgModuleName
  -> m [Module]
loadAll info cache pkgModuleName = do
  modul <- load info cache pkgModuleName
  deps  <- mapM (loadAll info cache) (dependencies modul)
  let modul' = expandExports modul
  pure $ modul' : concat deps

load
  :: (MonadError Error m, MonadIO m)
  => PackageInfo
  -> ModuleCache
  -> PkgModuleName
  -> m Module
load info cache name@(PkgModuleName pkgName _) = do
  path   <- filePath info name
  cache' <- liftIO $ readIORef cache
  case Map.lookup path cache' of
    Just m -> pure m
    _      -> do
      file <- readFile' path
      let m = parseKiteFile path pkgName file
      case m of
        Left  err -> throwError $ ParseError err
        Right m'  -> do
          liftIO $ atomicModifyIORef' cache $ \c -> (Map.insert path m' c, m')

-- We skip any references to Kite.Primitive because it's not a normal module.
-- It has no corresponding file and its definitions are automatically in scope
-- anyway.
dependencies :: Module_ n (Expr n ty) ty -> [PkgModuleName]
dependencies Module { moduleImports = imports } =
  filter (/= modPrim) $ nub $ map importName imports

-- | Attempt to construct a file path to the given module.
-- If the module is in our own package, use the root package directory.
-- Otherwise, look up the path in the 'PackageInfo'.
filePath :: MonadError Error m => PackageInfo -> PkgModuleName -> m FilePath
filePath info (PkgModuleName pkgName (ModuleName components)) = do
  dir <- if pkgName == packageName info
    then pure $ packageRootDir info
    else case Map.lookup pkgName (packageDeps info) of
      Just p  -> pure p
      Nothing -> throwError $ UnknownPackage pkgName
  pure $ dir <> "/" <> intercalate "/" components <> ".kite"

-- Sorts a set of modules in dependency order. Each module will only depend on
-- modules before it in the list. Returns an error if there are cyclic
-- dependencies.
sortModules :: MonadError Error m => [Module_ n e ty] -> m [Module_ n e ty]
sortModules ms =
  let adjList =
        map (\m -> (m, moduleName m, map importName (moduleImports m))) ms
      graph          = stronglyConnCompR adjList
      cycles         = flattenSCCs (filter isCyclic graph)
      orderedModules = map fst3 (flattenSCCs graph)
  in  case cycles of
        [] -> pure orderedModules
        _  -> throwError ModuleImportCycle

isCyclic :: SCC a -> Bool
isCyclic = \case
  CyclicSCC  _ -> True
  AcyclicSCC _ -> False

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
