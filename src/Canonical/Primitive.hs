module Canonical.Primitive where

import           ELC.Primitive                  ( modPrim )
import           Syn

primitives :: [(RawName, ModuleName)]
primitives = map (, modPrim) $ miscFunctions <> numFunctions <> list <> types

miscFunctions :: [RawName]
miscFunctions = ["error", "appendString"]

numFunctions, list, types :: [RawName]
numFunctions = ["+", "*", "-"]
list = ["::", "[]"]
types = ["Int", "String"]
