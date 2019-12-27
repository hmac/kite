module ModuleLoader where

import           System.Directory               ( getCurrentDirectory )

import           Syn.Parse                      ( parseLamFile )
import           Syntax
import           Data.List                      ( nub
                                                , intercalate
                                                , sortBy
                                                , permutations
                                                )
import qualified Canonical                     as Can
import           Canonicalise                   ( canonicaliseModule )

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

-- With canonicalisation we can probably flatten this tree.
--                               the module   its dependencies
data ModuleGroup = ModuleGroup (Can.Module Can.Exp) [ModuleGroup]
  deriving (Show)

-- like loadFromPath but returns a flat list of modules, in dependency order
loadFromPath' :: FilePath -> IO (Either String [Can.Module Can.Exp])
loadFromPath' path = do
  result <- loadFromPath path
  pure $ case result of
    Left err -> Left err
    Right (ModuleGroup m deps) ->
      (++ [m]) <$> sortModules (nub (concatMap flatten deps))

loadFromPath :: FilePath -> IO (Either String ModuleGroup)
loadFromPath path = do
  root  <- getCurrentDirectory
  modul <- fmap canonicaliseModule . parseLamFile <$> readFile path
  case modul of
    Left  err -> pure (Left err)
    Right m   -> do
      deps <- mapM (loadAll root) (dependencies m)
      case sequence deps of
        Left  err   -> pure (Left err)
        Right deps' -> pure $ Right $ ModuleGroup m deps'

loadAll :: FilePath -> ModuleName -> IO (Either String ModuleGroup)
loadAll root name = do
  modul <- fmap canonicaliseModule <$> load root name
  case modul of
    Left  err -> pure (Left err)
    Right m   -> do
      deps <- mapM (loadAll root) (dependencies m)
      case sequence deps of
        Left  err   -> pure (Left err)
        Right deps' -> pure $ Right $ ModuleGroup m deps'

load :: FilePath -> ModuleName -> IO (Either String (Module Syn))
load root name = parseLamFile <$> readFile (filePath root name)

flatten :: ModuleGroup -> [Can.Module Can.Exp]
flatten (ModuleGroup m []  ) = [m]
flatten (ModuleGroup m deps) = m : concatMap flatten deps

-- We skip any references to Lam.Primitive because it's not a normal module.
-- It has no corresponding file and its definitions are automatically in scope
-- anyway.
dependencies :: Module_ n (Syn_ n) -> [ModuleName]
dependencies Module { moduleImports = imports } =
  filter (/= ModuleName ["Lam", "Primitive"]) $ nub $ map importName imports

filePath :: FilePath -> ModuleName -> FilePath
filePath root (ModuleName components) =
  root <> "/" <> intercalate "/" components <> ".lam"

-- Sorts a set of modules in dependency order. Each module will only depend on
-- modules before it in the list. Returns an error if there are cyclic
-- dependencies.
sortModules :: [Can.Module Can.Exp] -> Either String [Can.Module Can.Exp]
sortModules []  = Right []
sortModules [m] = Right [m]
sortModules ms =
  let pairs = map (take 2) (permutations ms)
  in  if any (\[a, b] -> a `dependsOn` b && b `dependsOn` a) pairs
        then Left "Cyclic dependency detected"
        else Right $ sortBy compareModules ms

compareModules :: Can.Module Can.Exp -> Can.Module Can.Exp -> Ordering
compareModules m1 m2 = case (m1 `dependsOn` m2, m2 `dependsOn` m1) of
  (True , False) -> GT
  (False, True ) -> LT
  (False, False) -> EQ
  (True , True ) -> EQ

dependsOn :: Can.Module Can.Exp -> Can.Module Can.Exp -> Bool
m1 `dependsOn` m2
  | any (\i -> importName i == moduleName m2) (moduleImports m1) = True
  | otherwise = False
