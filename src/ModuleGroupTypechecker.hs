module ModuleGroupTypechecker where

import           Data.Name                      ( Name )
import           Constraint                     ( LocatedError )
import           Constraint.Generate.M          ( run )
import           Constraint.Generate.Module     ( generateModule )
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
import qualified Constraint.Primitive
import           ModuleGroup
import           Util

-- This module takes a list of modules from the loader and attempts to typecheck
-- them.

-- At this point the module has already been canonicalised, so we can assume
-- that all names are unique and not worry about clashes. We also assume that
-- the modules are in dependency order (this is handled by the ModuleLoader).

typecheckModuleGroup
  :: UntypedModuleGroup -> Either LocatedError TypedModuleGroup
typecheckModuleGroup (ModuleGroup m deps) = do
  -- Typecheck each module in order, so that definitions from imported modules
  -- are available when the module is typechecked.
  -- This requires that modules in a group have a total order (see
  -- ModuleLoader).

  let ms       = deps ++ [m]
  let (res, _) = run $ mapAccumLM generateModule Constraint.Primitive.env ms
  (_env', typedModules) <- res
  case reverse typedModules of
    (typedModule : typedDeps) ->
      pure $ TypedModuleGroup typedModule (reverse typedDeps)
    [] -> error "ModuleGroupTypechecker: empty list found"

-- Same as above but using the new typechecker.
-- We return an UntypedModuleGroup because the new typechecker doesn't yet add
-- type annotations when it checks things.
typecheckModuleGroup2
  :: UntypedModuleGroup -> Either Type.LocatedError UntypedModuleGroup
typecheckModuleGroup2 (ModuleGroup m deps) = do
  let ms = deps ++ [m]
  (_env', typedModules) <- Type.runTypeM Type.defaultTypeEnv
    $ mapAccumLM checkModule mempty ms
  case reverse typedModules of
    (typedModule : typedDeps) ->
      pure $ ModuleGroup typedModule (reverse typedDeps)
    [] -> error "ModuleGroupTypechecker: empty list found"

dumpEnv
  :: UntypedModuleGroup
  -> Either Type.LocatedError (Type.Ctx, [(Name, Type.Type, Type.Exp)])
dumpEnv (ModuleGroup m deps) =
  Type.runTypeM Type.defaultTypeEnv $ mconcatMapM translateModule (deps <> [m])
