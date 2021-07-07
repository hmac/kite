-- TODO: merge this module with Prim, if possible.
module Type.Primitive where

import           AST                            ( ConMeta(..) )
import           Data.Name                      ( Name
                                                , prim
                                                )
import           Data.String                    ( fromString )
import           Type.Type                      ( Ctx
                                                , CtxElem(..)
                                                , Type(..)
                                                , U(..)
                                                , V(..)
                                                )

primCtx :: Ctx
primCtx = primitiveConstructors <> primitiveFns

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

string :: Type
string = TCon (prim "String") []

int :: Type
int = TCon (prim "Int") []

char :: Type
char = TCon (prim "Char") []

bool :: Type
bool = TCon (prim "Bool") []

unit :: Type
unit = TCon (prim "Unit") []

list :: Type -> Type
list a = TCon (prim "List") [a]

io :: Type -> Type
io a = TCon (prim "IO") [a]

primTypeCtx :: [(Name, ())]
primTypeCtx = map
  (\n -> (prim n, ()))
  [ "String"
  , "Int"
  , "Char"
  , "Bool"
  , "Unit"
  , "List"
  , "IO"
  , "Tuple2"
  , "Tuple3"
  , "Tuple4"
  , "Tuple5"
  , "Tuple6"
  , "Tuple7"
  , "Tuple8"
  ]

primitiveConstructors :: Ctx
primitiveConstructors =
  [ V (Free (prim "Unit"))  unit
  , V (Free (prim "True"))  bool
  , V (Free (prim "False")) bool
  , V (Free (prim "[]")) (Forall (U 0 "a") (list (UType (U 0 "a"))))
  , V
    (Free (prim "::"))
    (Forall
      (U 0 "a")
      (Fn (UType (U 0 "a"))
          (Fn (list (UType (U 0 "a"))) (list (UType (U 0 "a"))))
      )
    )
  , V (Free (prim "Tuple2")) (mkTupleCon 2 (prim "Tuple2"))
  , V (Free (prim "Tuple3")) (mkTupleCon 3 (prim "Tuple3"))
  , V (Free (prim "Tuple4")) (mkTupleCon 4 (prim "Tuple4"))
  , V (Free (prim "Tuple5")) (mkTupleCon 5 (prim "Tuple5"))
  , V (Free (prim "Tuple6")) (mkTupleCon 6 (prim "Tuple6"))
  , V (Free (prim "Tuple7")) (mkTupleCon 7 (prim "Tuple7"))
  , V (Free (prim "Tuple8")) (mkTupleCon 8 (prim "Tuple8"))
  , V
    (Free (prim "MkIO"))
    (Forall (U 0 "a")
            (Fn (Fn (Fn (UType (U 0 "a")) unit) unit) (io (UType (U 0 "a"))))
    )
  ]

-- Primitive functions
-- Note: the type of unconsChar is weird because we want to implement it
-- without any knowledge of the runtime structure of complex types like tuples
-- or Maybe, since that may change in the future.
primitiveFns :: Ctx
primitiveFns =
  [ V (Free (prim "appendString")) (Fn string (Fn string string))
  , V (Free (prim "$chars"))       (Fn string (list char))
  , V (Free (prim "$consChar"))    (Fn char (Fn string string))
  -- unconsChar : String -> a -> (Char -> String -> a) -> a
  , let a = U 0 "a"
    in
      V
        (Free (prim "$unconsChar"))

        (Forall
          a
          (Fn string
              (Fn (UType a) (Fn (Fn char (Fn string (UType a))) (UType a)))
          )
        )
  , V (Free (prim "+"))         (Fn int (Fn int int))
  , V (Free (prim "-"))         (Fn int (Fn int int))
  , V (Free (prim "*"))         (Fn int (Fn int int))
  , V (Free (prim "/"))         (Fn int (Fn int int))
  , V (Free (prim "$showInt"))  (Fn int string)
  , V (Free (prim "$showChar")) (Fn char string)
  , V (Free (prim "$eqInt"))    (Fn int (Fn int bool))
  , V (Free (prim "$eqChar"))   (Fn char (Fn char bool))
  -- readInt : String -> a -> (Int -> a) -> a
  , let a = U 0 "a"
    in  V
          (Free (prim "$readInt"))

          (Forall a (Fn string (Fn (UType a) (Fn (Fn int (UType a)) (UType a))))
          )
  ]

mkTupleCon :: Int -> Name -> Type
mkTupleCon len tcon =
  let us = map (uncurry U) $ take len $ zip
        [0 ..]
        (map (fromString . (: [])) ['a' ..])
  in  foldr Forall (foldr (Fn . UType) (TCon tcon (map UType us)) us) us
