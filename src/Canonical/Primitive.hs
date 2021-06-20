module Canonical.Primitive where

import           Syn

modPrim :: ModuleName
modPrim = ModuleName ["Kite", "Primitive"]

primitives :: [(RawName, ModuleName)]
primitives = map (, modPrim) $ miscFunctions <> numFunctions <> list <> io

miscFunctions :: [RawName]
miscFunctions = ["error", "appendString"]

numFunctions, list :: [RawName]
numFunctions = ["+", "*", "-"]
list = ["::", "[]"]

io :: [RawName]
io = ["IO"]
