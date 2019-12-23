module ModuleGroupTypechecker where

import           Util
import           Syntax
import qualified Canonical                     as Can
import           ModuleLoader                   ( ModuleGroup(..) )
import           Typecheck.THIH                 ( Id
                                                , Type
                                                , Expl
                                                , Scheme
                                                , ClassEnv
                                                , Assump(..)
                                                , Alt
                                                , tiProgram
                                                , Error
                                                )
import           Typecheck.Desugar              ( desugarModule )
import           Typecheck.Translate            ( typeConstructors
                                                , dataConstructors
                                                , toBindGroup
                                                , addTypeclasses
                                                , addInstances
                                                , translateInstance
                                                , instanceMethods
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
  let
    (_types, datacons, funs, typeclasses, instancemethods, methods, env, classEnv)
      = extractDecls g Prim.classEnv
    assumps = map (uncurry (:>:)) (datacons <> Prim.constructors) <> methods
  in
    do
      instancemethods' <- instancemethods
      ce               <- classEnv
      case tiProgram ce assumps [(funs <> instancemethods', [])] of
        Left  e -> Left e
        Right _ -> Right ()

dumpEnv
  :: ModuleGroup -> ([Can.Typeclass], Either Error [Expl], [Assump], [Expl])
dumpEnv g =
  let (_types, datacons, funs, typeclasses, instancemethods, methods, _, _) =
          extractDecls g Prim.classEnv
      assumps = map (uncurry (:>:)) (datacons <> Prim.constructors) <> methods
  in  (typeclasses, instancemethods, assumps, funs)

-- TODO: this has become very horrible. make less horrible.
extractDecls
  :: ModuleGroup
  -> Either Error ClassEnv
  -> ( [(Id, Type)]
     , [(Id, Scheme)]
     , [(Id, Scheme, [Alt])]
     , [Can.Typeclass]
     , Either Error [Expl]
     , [Assump]
     , ([(Id, Type)], [(Id, Scheme)])
     , Either Error ClassEnv
     )
extractDecls (ModuleGroup m deps) ce =
  let
    (deptypes, depdatacons, depfuns, deptypeclasses, depinstancemethods, depmethods, classEnv)
      = foldl
        (\(a, b, c, d, e, f, ce) g ->
          let (a', b', c', d', e', f', _, ce') = extractDecls g ce
          in  (a <> a', b <> b', c <> c', d <> d', e <> e', f <> f', ce')
        )
        ([], [], [], [], Right [], [], ce)
        deps
    core  = desugarModule m
    types = typeConstructors core
    datacons =
      dataConstructors (deptypes <> types <> Prim.typeConstructors, []) core
    depfuntypes = map (\(id_, scheme, _) -> (id_, scheme)) depfuns
    env =
      ( deptypes <> types <> Prim.typeConstructors
      , depdatacons <> datacons <> depfuntypes <> Prim.constructors
      )
    funs        = concatMap fst $ mapMaybe (toBindGroup env) (moduleDecls core)
    -- todo: I think nub can be removed here
    typeclasses = nub (deptypeclasses <> typeclassDecls core)
    instances   = map (translateInstance env) (instanceDecls core)
    methods     = depmethods <> concatMap (typeclassMethods env) typeclasses
    classEnv' =
      classEnv
        >>= addTypeclasses env (typeclassDecls core)
        >>= addInstances instances
    instancemethods = classEnv' >>= \ce ->
      pure (concatMap (instanceMethods ce env) (instanceDecls core))
  in
    ( nub (deptypes <> types)
    , nub (depdatacons <> datacons)
    , nub (depfuns <> funs)
    , typeclasses
    , instancemethods
    , nub (depmethods <> methods)
    , env
    , classEnv'
    )
