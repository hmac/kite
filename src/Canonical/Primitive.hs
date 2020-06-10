module Canonical.Primitive where

import           ELC.Primitive                  ( modPrim )
import           Syn

primitives :: [(RawName, ModuleName)]
primitives = map (, modPrim) $ miscFunctions <> numFunctions <> list <> io

miscFunctions :: [RawName]
miscFunctions = ["error", "appendString"]

numFunctions, list :: [RawName]
numFunctions = ["+", "*", "-"]
list = ["::", "[]"]

io :: [RawName]
io = ["IO"]
