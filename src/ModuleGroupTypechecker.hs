module ModuleGroupTypechecker where

import           Data.Name                      ( Name )
import           Type.Module                    ( checkModule
                                                , translateModule
                                                )
import qualified Type                           ( LocatedError
                                                , runTypeM
                                                , defaultTypeEnv
                                                , Ctx
                                                , Type
                                                , Exp
                                                )
import           ModuleGroup
import           Util

-- This module takes a list of modules from the loader and attempts to typecheck
-- them.

-- At this point the module has already been canonicalised, so we can assume
-- that all names are unique and not worry about clashes. We also assume that
-- the modules are in dependency order (this is handled by the ModuleLoader).

-- Typecheck each module in order, so that definitions from imported modules
-- are available when the module is typechecked.
-- This requires that modules in a group have a total order (see
-- ModuleLoader).
typecheckModuleGroup
  :: UntypedModuleGroup -> Either Type.LocatedError TypedModuleGroup
typecheckModuleGroup (ModuleGroup m deps) = do
  let ms = deps ++ [m]
  (_env', typedModules) <- Type.runTypeM Type.defaultTypeEnv
    $ mapAccumLM checkModule mempty ms
  case reverse typedModules of
    (typedModule : typedDeps) ->
      pure $ TypedModuleGroup typedModule (reverse typedDeps)
    [] -> error "ModuleGroupTypechecker: empty list found"

dumpEnv
  :: UntypedModuleGroup
  -> Either Type.LocatedError (Type.Ctx, [(Name, Type.Type, Type.Exp)])
dumpEnv (ModuleGroup m deps) =
  Type.runTypeM Type.defaultTypeEnv $ mconcatMapM translateModule (deps <> [m])
