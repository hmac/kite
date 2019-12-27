module ModuleGroupTypechecker where

import           Control.Monad                  ( void )
import           Data.Foldable                  ( foldlM )
import           Syntax
import qualified Canonical                     as Can
import           Typecheck.THIH                 ( Expl
                                                , ClassEnv
                                                , Assump(..)
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
                                                , Env
                                                )
import qualified Typecheck.Primitive           as Prim
import           Data.Maybe                     ( mapMaybe )

-- This module takes a ModuleGroup from the loader and attempts to typecheck it
-- and its dependent modules.

-- At this point the module has already been canonicalised, so we can assume
-- that all names are unique and not worry about clashes.

typecheckModules :: [Can.Module Can.Exp] -> Either Error ()
typecheckModules ms = do
  (classEnv, (_tycons, datacons), funs, methods) <- buildEnv ms
  let assumps = map (uncurry (:>:)) datacons <> methods
  void $ tiProgram classEnv assumps [(funs, [])]

dumpEnv :: [Can.Module Can.Exp] -> Either Error (Env, [Expl], [Assump])
dumpEnv ms = do
  (_ce, env, funs, methods) <- buildEnv ms
  pure (env, funs, methods)

buildEnv
  :: [Can.Module Can.Exp] -> Either Error (ClassEnv, Env, [Expl], [Assump])
buildEnv ms = do
  classEnv <- Prim.classEnv
  foldlM f (classEnv, (Prim.typeConstructors, Prim.constructors), [], []) ms
 where
  f
    :: (ClassEnv, Env, [Expl], [Assump])
    -> Can.Module Can.Exp
    -> Either Error (ClassEnv, Env, [Expl], [Assump])
  f (ce, (tycons, datacons), funs, typeclassmethods) m = do
    let core = desugarModule m
        env =
          ( tycons <> typeConstructors core
          , datacons
            <> dataConstructors (tycons <> typeConstructors core, []) core
          )
        funs' = concatMap fst $ mapMaybe (toBindGroup env) (moduleDecls core)
        instances = map (translateInstance env) (instanceDecls core)

    -- Extend the class env
    ce' <- addTypeclasses env (typeclassDecls core) ce
      >>= addInstances instances

    -- Extract instance methods, e.g. $Int$show : Int -> String
    let instancemethods =
          concatMap (instanceMethods ce' env) (instanceDecls core)

        -- Extract typeclass methods, e.g. show : Show a => a -> String
        methods' = typeclassmethods
          <> concatMap (typeclassMethods env) (typeclassDecls core)

    pure (ce', env, funs' <> funs <> instancemethods, methods')
