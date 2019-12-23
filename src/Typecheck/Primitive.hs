module Typecheck.Primitive
  ( constructors
  , typeConstructors
  , classEnv
  , listNil
  , listCons
  , tuple2
  , tuple3
  , tuple4
  , tuple5
  , tuple6
  , tuple7
  )
where

import           Typecheck.THIH
import           Control.Monad                  ( (>=>) )
import           Canonical                      ( Name(..) )

-- Primitive tuple constructor types
tuple2 :: Assump
tuple2 = TopLevel modPrim "$prim_tuple2" :>: Forall
  [Star, Star]
  ([] :=> (TGen 0 `fn` TGen 1 `fn` foldl TAp tTuple2 (map TGen [0 .. 1])))
tuple3 :: Assump
tuple3 = TopLevel modPrim "$prim_tuple3" :>: Forall
  [Star, Star, Star]
  (   []
  :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` foldl TAp
                                                 tTuple3
                                                 (map TGen [0 .. 2])
      )
  )
tuple4 :: Assump
tuple4 = TopLevel modPrim "$prim_tuple4" :>: Forall
  [Star, Star, Star, Star]
  (   []
  :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` foldl
        TAp
        tTuple4
        (map TGen [0 .. 3])
      )
  )
tuple5 :: Assump
tuple5 = TopLevel modPrim "$prim_tuple5" :>: Forall
  [Star, Star, Star, Star, Star]
  (   []
  :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` foldl
        TAp
        tTuple5
        (map TGen [0 .. 4])
      )
  )
tuple6 :: Assump
tuple6 = TopLevel modPrim "$prim_tuple6" :>: Forall
  [Star, Star, Star, Star, Star, Star]
  (   []
  :=> (    TGen 0
      `fn` TGen 1
      `fn` TGen 2
      `fn` TGen 3
      `fn` TGen 4
      `fn` TGen 5
      `fn` foldl TAp tTuple6 (map TGen [0 .. 5])
      )
  )
tuple7 :: Assump
tuple7 = TopLevel modPrim "$prim_tuple7" :>: Forall
  [Star, Star, Star, Star, Star, Star, Star]
  (   []
  :=> (    TGen 0
      `fn` TGen 1
      `fn` TGen 2
      `fn` TGen 3
      `fn` TGen 4
      `fn` TGen 5
      `fn` TGen 6
      `fn` foldl TAp tTuple7 (map TGen [0 .. 6])
      )
  )

-- primitive List constructors
listNil :: Assump
listNil = TopLevel modPrim "[]" :>: Forall [Star] ([] :=> TAp tList (TGen 0))
listCons :: Assump
listCons = TopLevel modPrim "::" :>: Forall
  [Star]
  ([] :=> (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))

-- primitive Bool constructors
-- We need these because the comparison functions (>, <, etc.) have a return
-- type of Bool and they are primitives.
-- Maybe we can change their return type to Data.Bool.Bool or something?
true :: Assump
true = TopLevel modPrim "True" :>: Forall [] ([] :=> tBool)
false :: Assump
false = TopLevel modPrim "False" :>: Forall [] ([] :=> tBool)

-- other primitive functions
primError :: Assump
primError =
  TopLevel modPrim "error" :>: Forall [Star] ([] :=> (tString `fn` TGen 0))
primConcatString :: Assump
primConcatString = TopLevel modPrim "concatString"
  :>: Forall [] ([] :=> (list tString `fn` tString))
primAppendString :: Assump
primAppendString = TopLevel modPrim "appendString"
  :>: Forall [] ([] :=> (tString `fn` tString `fn` tString))

-- This needs to change a bit.
-- We should have primitive show functions for each primitive type (Int, Float
-- etc.) and a standard Lam typeclass for Show.
-- The Show instances for the primitive types will then reference their
-- corresponding primitive show functions.
primShowInt :: Assump
primShowInt =
  TopLevel modPrim "showInt" :>: Forall [] ([] :=> (tInt `fn` tString))
primShowFloat :: Assump
primShowFloat =
  TopLevel modPrim "showFloat" :>: Forall [] ([] :=> (tFloat `fn` tString))

primNumPlus :: Assump
primNumPlus = TopLevel modPrim "+" :>: Forall
  [Star]
  ([IsIn numClass (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
primNumSub :: Assump
primNumSub = TopLevel modPrim "-" :>: Forall
  [Star]
  ([IsIn numClass (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
primNumMult :: Assump
primNumMult = TopLevel modPrim "*" :>: Forall
  [Star]
  ([IsIn numClass (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
primOrdLt :: Assump
primOrdLt = TopLevel modPrim "<"
  :>: Forall
        [Star]
        ([IsIn ordClass (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))

typeConstructors :: [(Id, Type)]
typeConstructors =
  [ (TopLevel modPrim "Int"   , tInt)
  , (TopLevel modPrim "String", tString)
  , (TopLevel modPrim "Float" , tFloat)
  ]

constructors :: [(Id, Scheme)]
constructors =
  let tuple (f :>: s) = (f, s)
  in  map
        tuple
        [ listCons
        , listNil
        , true
        , false
        , primError
        , primShowInt
        , primShowFloat
        , primConcatString
        , primAppendString
        , primNumPlus
        , primNumSub
        , primNumMult
        , primOrdLt
        ]

classEnv :: Either Error ClassEnv
classEnv = transform initialEnv
 where
  transform =
    addPreludeClasses
      >=> addInst [] (IsIn ordClass tUnit)
      >=> addInst [] (IsIn ordClass tChar)
      >=> addInst [] (IsIn ordClass tInt)
      >=> addInst
            [ IsIn ordClass (TVar (Tyvar (Local "a") Star))
            , IsIn ordClass (TVar (Tyvar (Local "b") Star))
            ]
            (IsIn
              ordClass
              (pair (TVar (Tyvar (Local "a") Star))
                    (TVar (Tyvar (Local "b") Star))
              )
            )
      >=> addInst [] (IsIn numClass tInt)
      -- >=> addShowInstances

-- TODO: in future I think we want to derive Show via generics
addShowInstances :: EnvTransformer
addShowInstances =
  addInst [] (IsIn showClass tString)
    >=> addInst [] (IsIn showClass tInt)
    >=> addInst [] (IsIn showClass tFloat)
    >=> addInst [] (IsIn showClass tChar)
    >=> addInst [] (IsIn showClass tInteger)
    >=> addInst [] (IsIn showClass tBool)
    >=> addInst [] (IsIn showClass tDouble)
    >=> addInst [] (IsIn showClass tUnit)

addPreludeClasses :: EnvTransformer
addPreludeClasses = addCoreClasses >=> addNumClasses

-- TODO: fill in methods, or move these to Lam code
addCoreClasses :: EnvTransformer
addCoreClasses =
  addClass
      eqClass
      []
      [(Local "==", Forall [Star] ([] :=> (TGen 0 `fn` TGen 0 `fn` tBool)))]
    >=> addClass ordClass                     [eqClass] []
    >=> addClass (TopLevel modPrim "Read")    []        []
    >=> addClass (TopLevel modPrim "Bounded") []        []
    >=> addClass (TopLevel modPrim "Enum")    []        []
    >=> addClass (TopLevel modPrim "Functor") []        []
    >=> addClass (TopLevel modPrim "Monad")   []        []

-- TODO: fill in methods, or move these to Lam code
addNumClasses :: EnvTransformer
addNumClasses =
  addClass numClass [eqClass] []
    >=> addClass fractionalClass [numClass]                []
    >=> addClass integralClass   [TopLevel modPrim "Enum"] []
