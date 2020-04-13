module ModuleGroupTypechecker where

import           ModuleLoader                   ( ModuleGroup(..)
                                                , UntypedModuleGroup
                                                )

import           Constraint                     ( Error )
import           Constraint.Generate.M          ( run
                                                , TypeEnv
                                                )
import           Constraint.Generate.Module     ( generateModule )
import qualified Constraint.Primitive
import           ModuleGroup
import           Util

-- This module takes a list of modules from the loader and attempts to typecheck
-- them.

-- At this point the module has already been canonicalised, so we can assume
-- that all names are unique and not worry about clashes. We also assume that
-- the modules are in dependency order (this is handled by the ModuleLoader).


typecheckModuleGroup :: UntypedModuleGroup -> Either Error TypedModuleGroup
typecheckModuleGroup (ModuleGroup m deps) = do
  -- Typecheck each module in order, so that definitions from imported modules
  -- are available when the module is typechecked.
  -- This requires that modules in a group have a total order (see
  -- ModuleLoader).

  let ms = deps ++ [m]
  let (res, _) = run $ mapAccumLM
        generateModule
        Constraint.Primitive.env
        ms
  (_env', typedModules) <- res
  let (typedModule : typedDeps) = reverse typedModules
  pure $ TypedModuleGroup typedModule (reverse typedDeps)

dumpEnv :: UntypedModuleGroup -> Either Error TypeEnv
dumpEnv (ModuleGroup m deps) = do
  let ms = deps ++ [m]
  let (res, _) = run $ mapAccumLM
        generateModule
        Constraint.Primitive.env
        ms
  (env', _) <- res
  pure env'
