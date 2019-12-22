module ModuleGraphTypechecker where

import           Util
import           Syntax
import           ModuleLoader                   ( LoadedModule(..) )
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

-- This module takes a LoadedModule from the loader and attempts to typecheck it
-- and its dependent modules.

-- The tricky part here is making sure that we resolve names such that
-- everything is in scope when we typecheck it.

-- Imagine these three modules:

-- module A                 module B                module C
-- import B                 import C
-- import C
--
-- a1 : Int                 b : Int                 c1 : Int
-- a1 = b                   b = c                   c1 = 1
--
-- a2 : String                                      c2 : String
-- a2 = c2                                          c2 = "hi"

-- To typecheck A, we need to load and typecheck B and C.
-- To typecheck B, we need to load and typecheck C.

-- We want to:
-- - only typecheck each module once
-- - typecheck everything that could influence the typing of the main module

-- Because we (currently) enforce top level type signatures on functions, we
-- are able to extract these out beforehand for each dependent module and so we
-- can typecheck modules in any order. If we allow omission of top level type
-- signatures in future, we may have to change this.

-- To typecheck a module M, the overall plan is:
-- 1. Fetch names and type sigs for all top level declarations in direct
--    dependent modules of M.
-- 2. Add these to the initial typing environment and run M through the
--    typechecker.
-- 3. Repeat for each dependent module, and their dependencies.

typecheckModule :: LoadedModule -> Either Error ()
typecheckModule lm =
  let (_tycons, datacons, depfuns, funs, typeclasses, methods) =
          extractDecls lm
      assumps = map (uncurry (:>:)) (datacons <> depfuns) <> methods
  in  do
        classEnv <- Prim.classEnv >>= addTypeclasses typeclasses
        case tiProgram classEnv assumps [(funs, [])] of
          Left  e -> Left e
          Right _ -> Right ()

dumpEnv :: LoadedModule -> ([Assump], Program)
dumpEnv lm =
  let (_tycons, datacons, depfuns, funs, _typeclasses, _methods) =
          extractDecls lm
      assumps = map (uncurry (:>:)) (datacons <> depfuns)
  in  (assumps, [(funs, [])])

extractDecls
  :: LoadedModule
  -> ( [(Id, Type)]
     , [(Id, Scheme)]
     , [(Id, Scheme)]
     , [(Id, Scheme, [Alt])]
     , [Typeclass]
     , [Assump]
     )
extractDecls (LoadedModule m deps) =
  let
    core = desugarModule m
    -- TODO: do something with typeclasses/methods in deps
    (deptycons, depdatacons, _, depfuns, _typeclasses, _depmethods) =
      concat6 (map extractDecls deps)
    depfunTypes = map (\(id_, scheme, _) -> (id_, scheme)) depfuns
    tycons :: [(Id, Type)]
    tycons = Prim.typeConstructors <> typeConstructors core
    datacons :: [(Id, Scheme)]
    datacons =
      Prim.constructors <> dataConstructors (deptycons <> tycons, []) core
    env = (deptycons <> tycons, depdatacons <> depfunTypes <> datacons)
    methods :: [Assump]
    methods = concatMap (typeclassMethods env) (typeclassDecls m)
    funs :: [(Id, Scheme, [Alt])]
    funs        = concatMap fst $ mapMaybe (toBindGroup env) (moduleDecls core)
    typeclasses = typeclassDecls core
  in
    (tycons, datacons, depfunTypes, funs, typeclasses, methods)

assumpToTuple :: Assump -> (Id, Scheme)
assumpToTuple (n :>: s) = (n, s)
