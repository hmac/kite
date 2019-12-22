module ModuleGroupTypechecker where

import           Util
import           Syntax
import qualified Canonical                     as Can
import           ModuleLoader                   ( ModuleGroup(..) )
import           Typecheck.THIH                 ( Id
                                                , Type
                                                , Scheme
                                                , Assump(..)
                                                , Alt
                                                , tiProgram
                                                , Error
                                                , Program
                                                )
import           Typecheck.Desugar              ( desugarModule )
import           Typecheck.Translate            ( typeConstructors
                                                , dataConstructors
                                                , toBindGroup
                                                , addTypeclasses
                                                , typeclassMethods
                                                )
import qualified Typecheck.Primitive           as Prim
import           Data.Maybe                     ( mapMaybe )

-- This module takes a ModuleGroup from the loader and attempts to typecheck it
-- and its dependent modules.

-- At this point the module has already been canonicalised, so we can assume
-- that all names are unique and not worry about clashes.

typecheckModule :: ModuleGroup -> Either Error ()
typecheckModule g =
  let (_types, datacons, funs, typeclasses, methods) = extractDecls' g
      assumps = map (uncurry (:>:)) (datacons <> Prim.constructors) <> methods
  in  do
        classEnv <- Prim.classEnv >>= addTypeclasses typeclasses
        case tiProgram classEnv assumps [(funs, [])] of
          Left  e -> Left e
          Right _ -> Right ()

dumpEnv :: ModuleGroup -> ([Assump], Program)
dumpEnv lm =
  let (_types, datacons, funs, _typeclasses, _methods) = extractDecls' lm
      assumps = map (uncurry (:>:)) (Prim.constructors <> datacons)
  in  (assumps, [(funs, [])])

extractDecls'
  :: ModuleGroup
  -> ( [(Id, Type)]
     , [(Id, Scheme)]
     , [(Id, Scheme, [Alt])]
     , [Can.Typeclass]
     , [Assump]
     )
extractDecls' (ModuleGroup m deps) =
  let (deptypes, depdatacons, depfuns, deptypeclasses, depmethods) =
          concat5 (map extractDecls' deps)
      core  = desugarModule m
      types = typeConstructors core
      datacons =
          dataConstructors (deptypes <> types <> Prim.typeConstructors, []) core
      depfuntypes = map (\(id_, scheme, _) -> (id_, scheme)) depfuns
      env =
          ( deptypes <> types <> Prim.typeConstructors
          , depdatacons <> datacons <> depfuntypes <> Prim.constructors
          )
      funs = concatMap fst $ mapMaybe (toBindGroup env) (moduleDecls core)
      typeclasses = deptypeclasses <> typeclassDecls core
      methods = depmethods <> concatMap (typeclassMethods env) typeclasses
  in  (types, datacons, funs, typeclasses, methods)
