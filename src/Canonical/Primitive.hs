module Canonical.Primitive where

import           ELC.Primitive                  ( modPrim )
import           Syntax                        as Syn

primitives :: [(Syn.Name, Syn.ModuleName)]
primitives = [add, mult, sub]

add, mult, sub :: (Syn.Name, Syn.ModuleName)
add = ("+", modPrim)
mult = ("*", modPrim)
sub = ("-", modPrim)
