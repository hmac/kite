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

-- Primitive tuple constructor types
tuple2 :: Assump
tuple2 = "$prim_tuple2" :>: Forall
  [Star, Star]
  ([] :=> (TGen 0 `fn` TGen 1 `fn` foldl TAp tTuple2 (map TGen [0 .. 1])))
tuple3 :: Assump
tuple3 = "$prim_tuple3" :>: Forall
  [Star, Star, Star]
  (   []
  :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` foldl TAp
                                                 tTuple3
                                                 (map TGen [0 .. 2])
      )
  )
tuple4 :: Assump
tuple4 = "$prim_tuple4" :>: Forall
  [Star, Star, Star, Star]
  (   []
  :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` foldl
        TAp
        tTuple4
        (map TGen [0 .. 3])
      )
  )
tuple5 :: Assump
tuple5 = "$prim_tuple5" :>: Forall
  [Star, Star, Star, Star, Star]
  (   []
  :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` foldl
        TAp
        tTuple5
        (map TGen [0 .. 4])
      )
  )
tuple6 :: Assump
tuple6 = "$prim_tuple6" :>: Forall
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
tuple7 = "$prim_tuple7" :>: Forall
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
listNil = "[]" :>: Forall [Star] ([] :=> TAp tList (TGen 0))
listCons :: Assump
listCons = "::" :>: Forall
  [Star]
  ([] :=> (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))

-- primitive Bool constructors
true :: Assump
true = "True" :>: Forall [] ([] :=> tBool)
false :: Assump
false = "False" :>: Forall [] ([] :=> tBool)

-- other primitive functions
primError :: Assump
primError = "error" :>: Forall [Star] ([] :=> (tString `fn` TGen 0))
primStringConcat :: Assump
primStringConcat =
  "$prim_stringconcat" :>: Forall [] ([] :=> (list tString `fn` tString))
-- TODO: add Show constraint
primShow :: Assump
primShow = "$prim_show"
  :>: Forall [Star] ([IsIn "Show" (TGen 0)] :=> (TGen 0 `fn` tString))
primNumPlus :: Assump
primNumPlus = "+" :>: Forall
  [Star]
  ([IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
primNumSub :: Assump
primNumSub = "-" :>: Forall
  [Star]
  ([IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
primNumMult :: Assump
primNumMult = "*" :>: Forall
  [Star]
  ([IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
primOrdLt :: Assump
primOrdLt = "<" :>: Forall
  [Star]
  ([IsIn "Ord" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))

typeConstructors :: [(Id, Type)]
typeConstructors = [("Int", tInt), ("String", tString), ("Bool", tBool)]

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
        , primShow
        , primStringConcat
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
      >=> addInst [] (IsIn "Ord" tUnit)
      >=> addInst [] (IsIn "Ord" tChar)
      >=> addInst [] (IsIn "Ord" tInt)
      >=> addInst
            [ IsIn "Ord" (TVar (Tyvar "a" Star))
            , IsIn "Ord" (TVar (Tyvar "b" Star))
            ]
            (IsIn "Ord" (pair (TVar (Tyvar "a" Star)) (TVar (Tyvar "b" Star))))
      >=> addInst [] (IsIn "Num" tInt)
      >=> addShowInstances

-- TODO: in future I think we want to derive Show via generics
addShowInstances :: EnvTransformer
addShowInstances =
  addInst [] (IsIn "Show" tString)
    >=> addInst [] (IsIn "Show" tInt)
    >=> addInst [] (IsIn "Show" tFloat)
    >=> addInst [] (IsIn "Show" tChar)
    >=> addInst [] (IsIn "Show" tInteger)
    >=> addInst [] (IsIn "Show" tBool)
    >=> addInst [] (IsIn "Show" tDouble)
    >=> addInst [] (IsIn "Show" tUnit)

addPreludeClasses :: EnvTransformer
addPreludeClasses = addCoreClasses >=> addNumClasses

addCoreClasses :: EnvTransformer
addCoreClasses =
  addClass "Eq" []
    >=> addClass "Ord"     ["Eq"]
    >=> addClass "Show"    []
    >=> addClass "Read"    []
    >=> addClass "Bounded" []
    >=> addClass "Enum"    []
    >=> addClass "Functor" []
    >=> addClass "Monad"   []

addNumClasses :: EnvTransformer
addNumClasses =
  addClass "Num" ["Eq", "Show"]
    >=> addClass "Real"       ["Num", "Ord"]
    >=> addClass "Fractional" ["Num"]
    >=> addClass "Integral"   ["Real", "Enum"]
    >=> addClass "RealFrac"   ["Real", "Fractional"]
    >=> addClass "Floating"   ["Fractional"]
    >=> addClass "RealFloat"  ["RealFrac", "Floating"]
