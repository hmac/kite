module ModuleGroupTypechecker where

import qualified Data.Map.Strict               as Map
import           ModuleLoader                   ( ModuleGroup(..) )

import           Syntax                  hiding ( Name )
import           Canonical                      ( Name(..) )
import           Constraint                     ( Error )
import           Constraint.Generate.M          ( run
                                                , Env
                                                , GenerateM
                                                )
import           Constraint.Generate.Bind       ( BindT(..) )
import           Constraint.Generate.Module     ( generateModule )
import qualified Constraint.Primitive
import           Util

-- This module takes a list of modules from the loader and attempts to typecheck
-- them.

-- At this point the module has already been canonicalised, so we can assume
-- that all names are unique and not worry about clashes. We also assume that
-- the modules are in dependency order (this is handled by the ModuleLoader).

typecheckModuleGroup :: ModuleGroup -> Either Error [BindT]
typecheckModuleGroup (ModuleGroup m deps) = do
  -- Typecheck each module in order, so that definitions from imported modules
  -- are available when the module is typechecked.
  -- This requires that modules in a group have a total order (see
  -- ModuleLoader).

  let ms       = deps ++ [m]
  let (res, _) = run $ mapAccumLM generate Constraint.Primitive.env ms
  (_env', moduleBinds) <- res
  pure (mconcat moduleBinds)

dumpEnv :: ModuleGroup -> Either Error Env
dumpEnv (ModuleGroup m deps) = do
  let ms       = deps ++ [m]
  let (res, _) = run $ mapAccumLM generate Constraint.Primitive.env ms
  (env', _) <- res
  pure env'

generate :: Env -> Module_ Name (Syn_ Name) -> GenerateM (Env, [BindT])
generate env modul = do
  binds <- generateModule env modul
  let env' = foldl (\e (BindT n _ sch) -> Map.insert n sch e) env binds
  pure (env', binds)
