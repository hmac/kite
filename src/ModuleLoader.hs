module ModuleLoader
  ( ModuleGroup(..)
  , UntypedModuleGroup
  , Error(..)
  , loadFromPath
  , loadFromPathAndRootDirectory
  )
where

import           System.Directory               ( getCurrentDirectory )

import           Syn.Parse                      ( parseLamFile )
import           Syn
import           Data.List                      ( nub
                                                , intercalate
                                                , sortBy
                                                , permutations
                                                )
import           Canonicalise                   ( canonicaliseModule )
import           ModuleGroup
import           ExpandImports                  ( expandImports )
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

data Error = ParseError String
           | NameError String
           | DependencyError String
           deriving (Eq, Show)

loadFromPath :: FilePath -> IO (Either Error UntypedModuleGroup)
loadFromPath path = do
  root <- getCurrentDirectory
  loadFromPathAndRootDirectory path root

loadFromPathAndRootDirectory
  :: FilePath -> FilePath -> IO (Either Error UntypedModuleGroup)
loadFromPathAndRootDirectory path root = do
  modul <- parseLamFile <$> readFile path
  case modul of
    Left  err -> pure (Left (ParseError err))
    Right m   -> do
      deps <- mapM (loadAll root) (dependencies m)
      pure (postProcess m deps)
 where
  postProcess
    :: Syn.Module Syn.Syn
    -> [Either Error [Syn.Module Syn.Syn]]
    -> Either Error UntypedModuleGroup
  postProcess m deps = do
    deps'      <- sequence deps
    sortedDeps <- sortModules (nub (concat deps'))
    let (expandedModule, expandedDeps) =
          expandImports (expandExports m) sortedDeps
    case canonicaliseModule expandedModule of
      Left  err -> Left (NameError (show err))
      Right cm  -> case mapM canonicaliseModule expandedDeps of
        Left  err   -> Left (NameError (show err))
        Right cdeps -> pure $ ModuleGroup cm cdeps

loadAll :: FilePath -> ModuleName -> IO (Either Error [Module Syn])
loadAll root name = do
  modul <- load root name
  case modul of
    Left  err -> pure (Left err)
    Right m   -> do
      deps <- mapM (loadAll root) (dependencies m)
      case sequence deps of
        Left  err   -> pure (Left err)
        Right deps' -> pure $ Right $ expandExports m : concat deps'

load :: FilePath -> ModuleName -> IO (Either Error (Module Syn))
load root name = do
  res <- parseLamFile <$> readFile (filePath root name)
  pure $ case res of
    Right r   -> Right r
    Left  err -> Left (ParseError err)

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
sortModules :: [Module_ n e ty] -> Either Error [Module_ n e ty]
sortModules []  = Right []
sortModules [m] = Right [m]
sortModules ms =
  let pairs = map (take 2) (permutations ms)
  in  if any (\[a, b] -> a `dependsOn` b && b `dependsOn` a) pairs
        then Left (DependencyError "Cyclic dependency detected")
        else Right $ sortBy compareModules ms

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
