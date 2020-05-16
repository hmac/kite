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
import           Data.List                      ( nub
                                                , intercalate
                                                , sortBy
                                                )
import           Canonicalise                   ( canonicaliseModule )
import           ModuleGroup
import           ExpandImports                  ( expandImports )
import qualified ExpandImports
import           ExpandExports                  ( expandExports )

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

loadAll :: FilePath -> ModuleName -> IO (Either String [Module Syn])
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

load :: FilePath -> ModuleName -> IO (Either String (Module Syn))
load root name = parseLamFile <$> readFile (filePath root name)

-- We skip any references to Lam.Primitive because it's not a normal module.
-- It has no corresponding file and its definitions are automatically in scope
-- anyway.
dependencies :: Module_ n (Syn_ n m c ty) ty -> [ModuleName]
dependencies Module { moduleImports = imports } =
  filter (/= ModuleName ["Lam", "Primitive"]) $ nub $ map importName imports

filePath :: FilePath -> ModuleName -> FilePath
filePath root (ModuleName components) =
  root <> "/" <> intercalate "/" components <> ".lam"

-- Sorts a set of modules in dependency order. Each module will only depend on
-- modules before it in the list. Returns an error if there are cyclic
-- dependencies.
sortModules
  :: (Eq n, Eq ty, Eq e) => [Module_ n e ty] -> Either String [Module_ n e ty]
sortModules []  = Right []
sortModules [m] = Right [m]
sortModules ms =
  let pairs = asPairs $ permutationsOf2 ms
  in  if any (\(a, b) -> a `dependsOn` b && b `dependsOn` a) pairs
        then Left "Cyclic dependency detected"
        else Right $ sortBy compareModules ms

asPairs :: [[a]] -> [(a, a)]
asPairs []            = []
asPairs ([x, y] : xs) = (x, y) : asPairs xs
asPairs (x : _) =
  error
    $  "ModuleLoader.asPairs: Expected list of length 2, found list of length "
    <> show (length x)

permutationsOf2 :: Eq a => [a] -> [[a]]
permutationsOf2 xs =
  filter ((== 2) . length . nub) $ mapM (const xs) [1 .. 2 :: Int]

compareModules :: Module_ n e ty -> Module_ n e ty -> Ordering
compareModules m1 m2 = case (m1 `dependsOn` m2, m2 `dependsOn` m1) of
  (True , False) -> GT
  (False, True ) -> LT
  (False, False) -> EQ
  (True , True ) -> EQ

dependsOn :: Module_ n e ty -> Module_ n e ty -> Bool
m1 `dependsOn` m2
  | any (\i -> importName i == moduleName m2) (moduleImports m1) = True
  | otherwise = False
