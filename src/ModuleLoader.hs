module ModuleLoader
  ( ModuleGroup(..)
  , module ModuleLoader
  -- , UntypedModuleGroup
  -- , loadFromPath
  -- , loadFromPathAndRootDirectory
  ) where

import           Data.IORef                     ( IORef
                                                , atomicModifyIORef'
                                                , newIORef
                                                , readIORef
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           System.Directory               ( getCurrentDirectory )

import           AST                            ( Expr )
import           Canonicalise                   ( canonicaliseModule )
import           Data.Graph                     ( SCC(..)
                                                , flattenSCCs
                                                , stronglyConnCompR
                                                )
import           Data.List                      ( intercalate )
import           Data.Name                      ( showModuleName )
import           ExpandExports                  ( expandExports )
import           ExpandImports                  ( expandImports )
import qualified ExpandImports
import           ModuleGroup
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

-- We use a global cache of parsed modules to prevent parsing the same file twice.
type ModuleCache = IORef (Map String (Either String Module))

-- TODO: structured errors
loadFromPath :: FilePath -> IO (Either String UntypedModuleGroup)
loadFromPath path = do
  root <- getCurrentDirectory
  loadFromPathAndRootDirectory path root

loadFromPathAndRootDirectory
  :: FilePath -> FilePath -> IO (Either String UntypedModuleGroup)
loadFromPathAndRootDirectory path root = do
  modul <- parseKiteFile path <$> readFile path

  case modul of
    Left  err -> pure (Left err)
    Right m   -> do
      -- Create a mutable map to store the contents of each module as we parse them.
      -- This stops us parsing the same module multiple times.
      cache <- newIORef (Map.singleton path modul)

      deps  <- mapM (loadAll root cache) (dependencies m)
      case sequence deps of
        Left  err   -> pure (Left err)
        Right deps' -> case sortModules (nub (concat deps')) of
          Left err -> pure (Left err)
          Right sortedDeps ->
            case expandImports (expandExports m) sortedDeps of
              Right (expandedModule, expandedDeps) -> pure $ pure $ ModuleGroup
                (canonicaliseModule expandedModule)
                (map canonicaliseModule expandedDeps)
              Left (ExpandImports.CannotFindModule importingModule missingModule)
                -> pure
                  $  Left
                  $  "In "
                  <> showModuleName importingModule
                  <> ", cannot find module "
                  <> showModuleName missingModule

loadAll :: FilePath -> ModuleCache -> ModuleName -> IO (Either String [Module])
loadAll root cache name = do
  modul <- load root cache name
  case modul of
    Left  err -> pure (Left err)
    Right m   -> do
      deps <- mapM (loadAll root cache) (dependencies m)
      case sequence deps of
        Left  err   -> pure (Left err)
        Right deps' -> do
          let m' = expandExports m
          pure $ Right $ m' : concat deps'

load :: FilePath -> ModuleCache -> ModuleName -> IO (Either String Module)
load root cache name = do
  let path = filePath root name
  Map.lookup path <$> readIORef cache >>= \case
    Just m -> pure m
    _      -> do
      m <- parseKiteFile path <$> readFile path
      atomicModifyIORef' cache $ \c -> (Map.insert path m c, m)

-- We skip any references to Kite.Primitive because it's not a normal module.
-- It has no corresponding file and its definitions are automatically in scope
-- anyway.
dependencies :: Module_ n (Expr n ty) ty -> [ModuleName]
dependencies Module { moduleImports = imports } =
  filter (/= ModuleName ["Kite", "Primitive"]) $ nub $ map importName imports

filePath :: FilePath -> ModuleName -> FilePath
filePath root (ModuleName components) =
  root <> "/" <> intercalate "/" components <> ".kite"

-- Sorts a set of modules in dependency order. Each module will only depend on
-- modules before it in the list. Returns an error if there are cyclic
-- dependencies.
sortModules :: [Module_ n e ty] -> Either String [Module_ n e ty]
sortModules ms =
  let adjList =
        map (\m -> (m, moduleName m, map importName (moduleImports m))) ms
      graph          = stronglyConnCompR adjList
      cycles         = flattenSCCs (filter isCyclic graph)
      orderedModules = map fst3 (flattenSCCs graph)
  in  case cycles of
        [] -> Right orderedModules
        _  -> Left "Cyclic dependency detected"

isCyclic :: SCC a -> Bool
isCyclic = \case
  CyclicSCC  _ -> True
  AcyclicSCC _ -> False

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
