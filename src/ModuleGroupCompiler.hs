{-# LANGUAGE DuplicateRecordFields #-}
module ModuleGroupCompiler where

-- This module takes a ModuleGroup, compiles each module in it, and somehow
-- merges it all together.

import           ModuleLoader                   ( ModuleGroup(..) )
import qualified ELC.Compile                   as ELC
import qualified LC.Compile                    as LC
import           Syntax
import qualified Canonical                     as Can


-- We'll attempt this as follows:
-- All top level declarations will be qualified with their module name.
-- When a module A imports module B, we'll insert top level declarations into A
-- which alias all the declarations in B. This approach should generalise well
-- to import aliases, qualification, hiding etc when we support them.

-- Example:
--
-- module A                 module B                  module C
-- import B                 import C
--
-- a = ...                  b = ...                   c = ...
-- b = B.b                  c = C.c                   a = ...
--                          a = C.a

data CompiledModule a = CompiledModule { cModuleName :: ModuleName
                                       , cModuleImports :: [Import]
                                       , cModuleExports :: [Can.Name]
                                       , cModuleEnv :: a
                                       , cModuleDeps :: [CompiledModule a]
                                       }
  deriving Show

compileModule :: ModuleGroup -> CompiledModule LC.Env
compileModule (ModuleGroup m deps) = CompiledModule
  { cModuleName    = moduleName m
  , cModuleImports = moduleImports m
  , cModuleExports = moduleExports m
  , cModuleEnv     = LC.runConvert (LC.convertEnv env)
  , cModuleDeps    = []
  }
  where env = foldl compileModule'' ELC.defaultEnv (deps ++ [m])

compileModule'' :: ELC.Env -> Can.Module Can.Exp -> ELC.Env
compileModule'' env m = LC.runConvert (ELC.translateModule env m)
