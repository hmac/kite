-- Primitive (built-in) functions and constructors.
-- These tend to be ones that either:
-- - use a syntax we don't support in general (e.g. ::)
-- - are compiled to custom backend-specific code (e.g. +)
module Prim where

import           Data.Name
import           Syn                            ( Module
                                                , Module_(..)
                                                )

name :: PkgModuleName
name = "kite.Kite.Prim"

moduleDefinition :: Module
moduleDefinition = Module { moduleName     = name
                          , moduleImports  = []
                          , moduleExports  = kitePrimitiveExports
                          , moduleDecls    = []
                          , moduleMetadata = []
                          }

-- These are functions which don't need importing to be used.
-- They are in scope globally and we don't expect the user to be able to shadow them.
globallyAvailablePrimitives :: [(RawName, PkgModuleName)]
globallyAvailablePrimitives =
  map (, name) $ listConstructors <> arithmetic <> comparison <> compose
 where
  listConstructors = ["::", "[]"]
  arithmetic       = ["+", "-", "/", "*"]
  -- TODO: ==
  comparison       = [">", "<", ">=", "<="]
  compose          = ["."]

kitePrimitiveExports :: [(RawName, [RawName])]
kitePrimitiveExports = [("IO", ["MkIO"])] <> map (, []) (builtins <> fcalls)
 where
  -- TODO: is there any reason to keep the $ prefix?
  -- As long as we namespace these under Kite.Prim, we should be OK right?
  builtins =
    [ "appendString"
    , "error"
    , "$chars"
    , "$consChar"
    , "$unconsChar"
    , "$showInt"
    , "$showChar"
    , "$eqInt"
    , "$eqChar"
    , "$readInt"
    ]
  fcalls = ["putStrLn", "putStr", "getLine"]
