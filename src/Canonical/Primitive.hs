{-# LANGUAGE TupleSections #-}
module Canonical.Primitive where

import           ELC.Primitive                  ( modPrim )
import           Syntax                        as Syn

primitives :: [(Syn.Name, Syn.ModuleName)]
primitives = map (, modPrim) ["+", "*", "-", "::", "[]"]
