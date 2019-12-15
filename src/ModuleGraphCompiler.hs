{-# LANGUAGE DuplicateRecordFields #-}
module ModuleGraphCompiler where

-- This module takes a LoadedModule, compiles each module in it, and somehow
-- merges it all together.

import           ModuleLoader                   ( LoadedModule(..) )
import qualified ELC.Compile                   as ELC
import qualified LC.Compile                    as LC
import           Syntax
import qualified Canonicalise                  as Can


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
                                       , cModuleExports :: [Name]
                                       , cModuleEnv :: a
                                       , cModuleDeps :: [CompiledModule a]
                                       }
  deriving Show

-- Recursively compiles a module and its dependencies to LC
compileModule :: LoadedModule -> CompiledModule LC.Env
compileModule l = compileToLC (compileModule' l)
 where
  compileToLC :: CompiledModule ELC.Env -> CompiledModule LC.Env
  compileToLC m = m { cModuleEnv  = LC.runConvert (LC.convertEnv (cModuleEnv m))
                    , cModuleDeps = map compileToLC (cModuleDeps m)
                    }

-- Recursively compiles a module and its dependencies to ELC
compileModule' :: LoadedModule -> CompiledModule ELC.Env
compileModule' (LoadedModule m deps) =
  let deps'    = map compileModule' deps
      depEnvs  = concatMap (ELC.collapseEnv . cModuleEnv) deps'
      elcEnv   = ELC.defaultEnv { ELC.imports = depEnvs }
      mcanon   = Can.canonicaliseModule m
      m'       = LC.runConvert (ELC.translateModule elcEnv mcanon)
      compiled = CompiledModule { cModuleName    = moduleName m
                                , cModuleEnv     = m'
                                , cModuleImports = moduleImports m
                                , cModuleExports = moduleExports m
                                , cModuleDeps    = deps'
                                }
  in  compiled
