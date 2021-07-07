module Canonical.Primitive where

import           Data.Name

modPrim :: PkgModuleName
modPrim = "kite.Kite.Primitive"

primitives :: [(RawName, PkgModuleName)]
primitives = map (, modPrim) $ miscFunctions <> numFunctions <> list <> io

miscFunctions :: [RawName]
miscFunctions = ["error", "appendString"]

numFunctions, list :: [RawName]
numFunctions = ["+", "*", "-", "/"]
list = ["::", "[]"]

io :: [RawName]
io = ["IO"]
