module Type.Primitive where

import           AST                            ( ConMeta(..) )
import           Data.Name                      ( Name
                                                , prim
                                                )

primitiveCtorInfo :: [(Name, ConMeta)]
primitiveCtorInfo =
  [(prim "[]", listNilMeta), (prim "::", listConsMeta), (prim "MkIO", mkIO)]

listNilMeta :: ConMeta
listNilMeta =
  ConMeta { conMetaTag = 0, conMetaArity = 0, conMetaTypeName = prim "List" }

listConsMeta :: ConMeta
listConsMeta =
  ConMeta { conMetaTag = 1, conMetaArity = 2, conMetaTypeName = prim "List" }

mkIO :: ConMeta
mkIO =
  ConMeta { conMetaTag = 0, conMetaArity = 1, conMetaTypeName = prim "IO" }
