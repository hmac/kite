-- TODO: merge this module with Prim, if possible.
module Type.Primitive where

import           AST                            ( ConMeta(..) )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Name                      ( Name
                                                , prim
                                                )
import           Data.String                    ( fromString )
import           Type.DSL                       ( fn
                                                , forAll
                                                , tcon
                                                , u_
                                                )
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
  [ ("putStrLn", (fn string unit))
  , ("putStr"  , (fn string unit))
  , ("getLine" , string)
  ]

primCtx :: Ctx
primCtx = primitiveConstructors <> primitivefns

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
string = tcon (prim "String") []

int :: Type
int = tcon (prim "Int") []

char :: Type
char = tcon (prim "Char") []

bool :: Type
bool = tcon (prim "Bool") []

unit :: Type
unit = tcon (prim "Unit") []

list :: Type -> Type
list a = tcon (prim "List") [a]

io :: Type -> Type
io a = tcon (prim "IO") [a]

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
  , V (prim "[]") (forAll (U 0 "a") (list (u_ (U 0 "a"))))
  , V
    (prim "::")
    (forAll
      (U 0 "a")
      (fn (u_ (U 0 "a")) (fn (list (u_ (U 0 "a"))) (list (u_ (U 0 "a")))))
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
    (forAll (U 0 "a")
            (fn (fn (fn (u_ (U 0 "a")) unit) unit) (io (u_ (U 0 "a"))))
    )
  ]

-- Primitive functions
-- Note: the type of unconsChar is weird because we want to implement it
-- without any knowledge of the runtime structure of complex types like tuples
-- or Maybe, since that may change in the future.
-- TODO: do we need to be generating globally unique U vars here?
primitivefns :: Ctx
primitivefns =
  [ V (prim "appendString") (fn string (fn string string))
  , V (prim "$chars")       (fn string (list char))
  , V (prim "$consChar")    (fn char (fn string string))
  -- unconsChar : String -> a -> (Char -> String -> a) -> a
  , let a = U 0 "a"
    in  V
          (prim "$unconsChar")

          (forAll
            a
            (fn string (fn (u_ a) (fn (fn char (fn string (u_ a))) (u_ a))))
          )
  , V (prim "+") (fn int (fn int int))
  , V (prim "-") (fn int (fn int int))
  , V (prim "*") (fn int (fn int int))
  , V (prim "/") (fn int (fn int int))
  , let a = U 0 "a"
        b = U 1 "b"
        c = U 2 "c"
    in  V
          (prim ".")
          (forAll
            b
            (forAll
              c
              (fn (fn (u_ b) (u_ c))
                  (forAll a (fn (fn (u_ a) (u_ b)) (fn (u_ a) (u_ c))))
              )
            )
          )
  , V (prim "$showInt")  (fn int string)
  , V (prim "$showChar") (fn char string)
  , V (prim "$eqInt")    (fn int (fn int bool))
  , V (prim "$eqChar")   (fn char (fn char bool))
  -- readInt : String -> a -> (Int -> a) -> a
  , let a = U 0 "a"
    in  V (prim "$readInt")

          (forAll a (fn string (fn (u_ a) (fn (fn int (u_ a)) (u_ a)))))
  ]

mkTupleCon :: Int -> Name -> Type
mkTupleCon len con =
  let us = map (uncurry U) $ take len $ zip
        [0 ..]
        (map (fromString . (: [])) ['a' ..])
  in  foldr forAll (foldr (fn . u_) (tcon con (map u_ us)) us) us
