module ModuleLoader
  ( ModuleGroup(..)
  , module ModuleLoader
  -- , UntypedModuleGroup
  -- , loadFromPath
  -- , loadFromPathAndRootDirectory
  )
where

import           System.Directory               ( getCurrentDirectory )

import           Data.Name                      ( showModuleName )
import           Syn.Parse                      ( parseLamFile )
import           Syn
import           Expr                           ( Expr )
import           Data.List                      ( intercalate )
import           Canonicalise                   ( canonicaliseModule )
import           ModuleGroup
import           ExpandImports                  ( expandImports )
import qualified ExpandImports
import           ExpandExports                  ( expandExports )
import           Data.Graph                     ( stronglyConnCompR
                                                , flattenSCCs
                                                , SCC(..)
                                                )
import           Util

-- This module is responsible for loading Lam modules. It should attempt to
-- cache modules so they're not loaded multiple times.

-- A typical Lam module might look like this:
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

-- TODO: structured errors
loadFromPath :: FilePath -> IO (Either String UntypedModuleGroup)
loadFromPath path = do
  root <- getCurrentDirectory
  loadFromPathAndRootDirectory path root

loadFromPathAndRootDirectory
  :: FilePath -> FilePath -> IO (Either String UntypedModuleGroup)
loadFromPathAndRootDirectory path root = do
  modul <- parseLamFile <$> readFile path
  case modul of
    Left  err -> pure (Left err)
    Right m   -> do
      deps <- mapM (loadAll root) (dependencies m)
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

loadAll :: FilePath -> ModuleName -> IO (Either String [Module])
loadAll root name = do
  modul <- load root name
  case modul of
    Left  err -> pure (Left err)
    Right m   -> do
      deps <- mapM (loadAll root) (dependencies m)
      case sequence deps of
        Left  err   -> pure (Left err)
        Right deps' -> do
          let m' = expandExports m
          pure $ Right $ m' : concat deps'

load :: FilePath -> ModuleName -> IO (Either String Module)
load root name = parseLamFile <$> readFile (filePath root name)

-- We skip any references to Lam.Primitive because it's not a normal module.
-- It has no corresponding file and its definitions are automatically in scope
-- anyway.
dependencies :: Module_ n (Expr n ty) ty -> [ModuleName]
dependencies Module { moduleImports = imports } =
  filter (/= ModuleName ["Lam", "Primitive"]) $ nub $ map importName imports

filePath :: FilePath -> ModuleName -> FilePath
filePath root (ModuleName components) =
  root <> "/" <> intercalate "/" components <> ".lam"

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
