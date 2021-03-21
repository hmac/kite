module ModuleGroupTypechecker where

import           Data.Name                      ( Name )
import           ModuleGroup
import qualified Type                           ( CtorInfo
                                                , Ctx
                                                , Exp
                                                , LocatedError
                                                , Type
                                                , defaultTypeEnv
                                                , runTypecheckM
                                                )
import           Type.Module                    ( checkModule
                                                , translateModule
                                                )
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
typecheckModuleGroup (ModuleGroup m deps) =
  Type.runTypecheckM Type.defaultTypeEnv $ do
    -- First typecheck the dependent modules
    (ctx, typedDeps  ) <- mapAccumLM checkModule mempty deps
    -- Then typecheck the main module
    (_  , typedModule) <- checkModule ctx m
    pure $ TypedModuleGroup typedModule typedDeps

dumpEnv
  :: UntypedModuleGroup
  -> Either
       Type.LocatedError
       (Type.Ctx, Type.CtorInfo, [(Name, Maybe Type.Type, Type.Exp)])
dumpEnv (ModuleGroup m deps) =
  Type.runTypecheckM Type.defaultTypeEnv
    $ mconcatMapM translateModule (deps <> [m])
