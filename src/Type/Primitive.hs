-- TODO: merge this module with Prim, if possible.
module Type.Primitive where

import           AST                            ( ConMeta(..) )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Name                      ( Name
                                                , prim
                                                )
import           Data.String                    ( fromString )
import           Type.Type                      ( Ctx
                                                , CtxElem(..)
                                                , Type(..)
                                                , U(..)
                                                )

-- In the future we will support syntax to declare the types of foreign calls,
-- like this:
--     foreign putStrLn : String -> IO ()
--     foreign getLine : IO String
--     foreign bindIO : IO a -> (a -> IO b) -> IO b
-- Until then, we hard-code the types here.
-- If you add a new fcall here, you also need to add its implementation in
-- Chez.Compile.
fcallInfo :: Map String Type
fcallInfo = Map.fromList
  -- name        type
  [ ("putStrLn", Fn string unit)
  , ("putStr"  , Fn string unit)
  , ("getLine" , string)
  ]

primCtx :: Ctx
primCtx = primitiveConstructors <> primitiveFns

primitiveCtorInfo :: Map Name ConMeta
primitiveCtorInfo =
  Map.fromList
    $ [(prim "[]", listNilMeta), (prim "::", listConsMeta), (prim "MkIO", mkIO)]
    <> tuples
 where
  tuples = map
    (\n ->
      let name = prim $ fromString $ "Tuple" <> show n
      in  ( name
          , ConMeta { conMetaTag = 0, conMetaArity = n, conMetaTypeName = name }
          )
    )
    [2 .. 8]

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

primTypeCtx :: Map Name ()
primTypeCtx = Map.fromList $ map
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
  [ V (prim "Unit")  unit
  , V (prim "True")  bool
  , V (prim "False") bool
  , V (prim "[]") (Forall (U 0 "a") (list (UType (U 0 "a"))))
  , V
    (prim "::")
    (Forall
      (U 0 "a")
      (Fn (UType (U 0 "a"))
          (Fn (list (UType (U 0 "a"))) (list (UType (U 0 "a"))))
      )
    )
  , V (prim "Tuple2") (mkTupleCon 2 (prim "Tuple2"))
  , V (prim "Tuple3") (mkTupleCon 3 (prim "Tuple3"))
  , V (prim "Tuple4") (mkTupleCon 4 (prim "Tuple4"))
  , V (prim "Tuple5") (mkTupleCon 5 (prim "Tuple5"))
  , V (prim "Tuple6") (mkTupleCon 6 (prim "Tuple6"))
  , V (prim "Tuple7") (mkTupleCon 7 (prim "Tuple7"))
  , V (prim "Tuple8") (mkTupleCon 8 (prim "Tuple8"))
  , V
    (prim "MkIO")
    (Forall (U 0 "a")
            (Fn (Fn (Fn (UType (U 0 "a")) unit) unit) (io (UType (U 0 "a"))))
    )
  ]

-- Primitive functions
-- Note: the type of unconsChar is weird because we want to implement it
-- without any knowledge of the runtime structure of complex types like tuples
-- or Maybe, since that may change in the future.
-- TODO: do we need to be generating globally unique U vars here?
primitiveFns :: Ctx
primitiveFns =
  [ V (prim "appendString") (Fn string (Fn string string))
  , V (prim "$chars")       (Fn string (list char))
  , V (prim "$consChar")    (Fn char (Fn string string))
  -- unconsChar : String -> a -> (Char -> String -> a) -> a
  , let a = U 0 "a"
    in
      V
        (prim "$unconsChar")

        (Forall
          a
          (Fn string
              (Fn (UType a) (Fn (Fn char (Fn string (UType a))) (UType a)))
          )
        )
  , V (prim "+") (Fn int (Fn int int))
  , V (prim "-") (Fn int (Fn int int))
  , V (prim "*") (Fn int (Fn int int))
  , V (prim "/") (Fn int (Fn int int))
  , let a = U 0 "a"
        b = U 1 "b"
        c = U 2 "c"
    in  V
          (prim ".")
          (Forall
            b
            (Forall
              c
              (Fn
                (Fn (UType b) (UType c))
                (Forall a (Fn (Fn (UType a) (UType b)) (Fn (UType a) (UType c)))
                )
              )
            )
          )
  , V (prim "$showInt")  (Fn int string)
  , V (prim "$showChar") (Fn char string)
  , V (prim "$eqInt")    (Fn int (Fn int bool))
  , V (prim "$eqChar")   (Fn char (Fn char bool))
  -- readInt : String -> a -> (Int -> a) -> a
  , let a = U 0 "a"
    in  V
          (prim "$readInt")

          (Forall a (Fn string (Fn (UType a) (Fn (Fn int (UType a)) (UType a))))
          )
  ]

mkTupleCon :: Int -> Name -> Type
mkTupleCon len tcon =
  let us = map (uncurry U) $ take len $ zip
        [0 ..]
        (map (fromString . (: [])) ['a' ..])
  in  foldr Forall (foldr (Fn . UType) (TCon tcon (map UType us)) us) us
