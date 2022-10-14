module Test.KiteCore.Compile where

import           AST                            ( ExprT(..)
                                                , Pat(..)
                                                )
import           Control.Monad.Identity         ( Identity )
import           Control.Monad.State.Strict     ( State
                                                , evalState
                                                , get
                                                , put
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Name                      ( Name )
import qualified Data.Set                      as Set
import           Data.String                    ( fromString )
import           KiteCore.Compile               ( liftAllLambdas
                                                , liftLambda
                                                , liftLet
                                                , liftLetsFromApplication
                                                )
import           Test.Diff                      ( shouldBe )
import           Test.Hspec              hiding ( shouldBe )
import           Type.Type                      ( Type(TCon, TOther)
                                                , Type'(Fn, IFn, UType)
                                                , U(..)
                                                )

test :: Spec
test = parallel $ do
  test_liftLambda
  test_liftAllLambdas
  test_liftLet
  test_liftLetsFromApplication

emptyGenName :: Identity Name
emptyGenName = undefined

withGenNameFromList :: [Name] -> (State [Name] Name -> State [Name] a) -> a
withGenNameFromList names f =
  let gen = do
        ns <- get
        let (hd : tail) = ns
        put tail
        pure hd
  in  evalState (f gen) names


test_liftLambda :: Spec
test_liftLambda = describe "liftLambda" $ do
  it "lifts a simple lambda" $ do
    let a = TOther $ UType (U 1 "a")
    liftLambda "f" (TOther (Fn a a)) [VarPat a "x"] (VarT a "x")
      `shouldBe` ([VarPat a "x"], VarT (TOther (Fn a a)) "f")
  it "lifts a lambda with a different return type" $ do
    -- (x y -> x) : a -> b -> a
    let a = TOther $ UType (U 1 "a")
        b = TOther $ UType (U 2 "b")
    liftLambda "f"
               (TOther (Fn a (TOther (Fn b a))))
               [VarPat a "x", VarPat b "y"]
               (VarT a "x")
      `shouldBe` ( [VarPat a "x", VarPat b "y"]
                 , VarT (TOther (Fn a (TOther (Fn b a)))) "f"
                 )
  it "lifts a lambda with a free variable" $ do
    let a = TOther $ UType (U 1 "a")
        b = TOther $ UType (U 2 "b")
    liftLambda "f" (TOther (Fn b a)) [VarPat b "y"] (VarT a "x")
      `shouldBe` ( [VarPat a "x", VarPat b "y"]
                 , AppT (TOther (Fn b a))
                        (VarT (TOther (Fn a (TOther (Fn b a)))) "f")
                        (VarT a "x")
                 )
  it "lifts a lambda with several bound and free variables" $ do
    let a       = TOther $ UType (U 1 "a")
        b       = TOther $ UType (U 2 "b")
        c       = TOther $ UType (U 3 "c")
        d       = TOther $ UType (U 3 "d")
        fType   = TOther $ Fn b (TOther (Fn d c))
        gType   = TOther $ Fn a d
        lamType = TOther (Fn a (TOther (Fn b c)))
        -- g :: a -> d
        -- f :: b -> d -> c
        -- func = (\x y -> f y (g x)) : a -> b -> c
        -- e = f y (g x) : c
        -- result = func g f
        e       = AppT c
                       (AppT (TOther (Fn d c)) (VarT fType "f") (VarT b "y"))
                       (AppT d (VarT (TOther (Fn a d)) "g") (VarT a "x"))
    let (bindings, application) =
          liftLambda "h" lamType [VarPat a "x", VarPat b "y"] e

    bindings
      `shouldBe` [ VarPat fType "f"
                 , VarPat gType "g"
                 , VarPat a     "x"
                 , VarPat b     "y"
                 ]
    application `shouldBe` AppT
      lamType
      (AppT (TOther (Fn gType lamType))
            (VarT (TOther (Fn fType (TOther (Fn gType lamType)))) "h")
            (VarT fType "f")
      )
      (VarT gType "g")
  it "lifts an implicit lambda with a free variable" $ do
    -- (y => x) : b => a ==> f(x, y) = x : a -> b -> a; f(x)
    let a = TOther $ UType (U 1 "a")
        b = TOther $ UType (U 2 "b")
    liftLambda "f" (TOther (IFn b a)) [VarPat b "y"] (VarT a "x")
      `shouldBe` ( [VarPat a "x", VarPat b "y"]
                 , AppT (TOther (Fn b a))
                        (VarT (TOther (Fn a (TOther (Fn b a)))) "f")
                        (VarT a "x")
                 )
  it "lifts a lambda with non-variable patterns" $ do
    -- (x True -> x)
    let a    = TOther $ UType (U 1 "a")
        bool = TCon "Bool" []
    liftLambda "f"
               (TOther (Fn a (TOther (Fn bool a))))
               [VarPat a "x", BoolPat bool True]
               (VarT a "x")
      `shouldBe` ( [VarPat a "x", BoolPat bool True]
                 , VarT (TOther (Fn a (TOther (Fn bool a)))) "f"
                 )

test_liftAllLambdas :: Spec
test_liftAllLambdas = describe "liftAllLambdas" $ do
  it "does nothing to an expression with no lambdas" $ do
    let a = TOther $ UType $ U 1 "a"
        e =
          LetT a [("x", VarT a "y", Nothing)] (AppT a (VarT a "f") (VarT a "x"))
    liftAllLambdas emptyGenName e `shouldBe` pure (e, [])
  it "lifts an expression with one lambda" $ do
    -- f (w => x) ==> g(x, w) = x; f (g x)
    -- w :: a
    -- x :: b
    -- f :: (a => b) -> c
    let a        = TOther $ UType $ U 1 "a"
        b        = TOther $ UType $ U 2 "b"
        c        = TOther $ UType $ U 3 "c"
        f1Type   = TOther (IFn a b)
        f1       = IAbsT f1Type (VarPat a "w") f1Type (VarT b "x")
        fType    = TOther $ Fn f1Type c
        e        = AppT c (VarT fType "f") f1
        -- IFn types get replaced by Fn types, but this is fine as we're past
        -- typechecking, and so implicits are all explicit anyway.
        f1Type'  = TOther $ Fn b $ TOther $ Fn a b
        expected = AppT
          c
          (VarT fType "f")
          (AppT (TOther (Fn a b)) (VarT f1Type' "g") (VarT b "x"))
        genName = pure "g"
    let (result, newBindings) = evalState (liftAllLambdas genName e) 1
    newBindings
      `shouldBe` [ ( "g"
                   , NE.singleton ([VarPat b "x", VarPat a "w"], VarT b "x")
                   )
                 ]
    result `shouldBe` expected
  it "lifts an expression with two lambdas" $ do
    -- f (w => x) (y => z) ==> f_1(x, w) = x; f_2(z, y) = z; f (f_1 x) (f_2 z)
    -- w :: a
    -- x :: b
    -- y :: c
    -- z :: d
    -- f :: (a => b) -> (c => d) -> e
    let a        = TOther $ UType $ U 1 "a"
        b        = TOther $ UType $ U 2 "b"
        c        = TOther $ UType $ U 3 "c"
        d        = TOther $ UType $ U 3 "d"
        e        = TOther $ UType $ U 4 "e"
        f1Type   = TOther (IFn a b)
        f1       = IAbsT f1Type (VarPat a "w") f1Type (VarT b "x")
        f2Type   = TOther (IFn c d)
        f2       = IAbsT f2Type (VarPat c "y") f2Type (VarT d "z")
        fType    = TOther $ Fn f1Type $ TOther $ Fn f2Type e
        f        = AppT e (AppT (TOther (Fn f2Type e)) (VarT fType "f") f1) f2
        -- IFn types get replaced by Fn types, but this is fine as we're past
        -- typechecking, and so implicits are all explicit anyway.
        f1Type'  = TOther $ Fn b $ TOther $ Fn a b
        f2Type'  = TOther $ Fn d $ TOther $ Fn c d
        fType'   = TOther $ Fn f1Type' $ TOther $ Fn f2Type' e
        expected = AppT
          e
          (AppT (TOther (Fn (TOther (IFn c d)) e))
                (VarT fType "f")
                (AppT (TOther (Fn a b)) (VarT f1Type' "f_1") (VarT b "x"))
          )
          (AppT (TOther (Fn c d)) (VarT f2Type' "f_2") (VarT d "z"))
        genName = do
          n <- get
          put (n + 1 :: Int)
          pure $ fromString $ "f_" <> show n
    let (result, newBindings) = evalState (liftAllLambdas genName f) 1
    newBindings
      `shouldBe` [ ( "f_1"
                   , NE.singleton ([VarPat b "x", VarPat a "w"], VarT b "x")
                   )
                 , ( "f_2"
                   , NE.singleton ([VarPat d "z", VarPat c "y"], VarT d "z")
                   )
                 ]
    result `shouldBe` expected
  it "lifts an expression with non-variable patterns" $ do
    -- f (g True -> g y) ==> f_1(y, g, True) = g y; f (f_1 y)
    -- y : a
    -- g : a -> b
    -- (g True -> g y) : (a -> b) -> Bool -> b
    -- f : ((a -> b) -> Bool -> b) -> c
    let
      a         = TOther $ UType $ U 1 "a"
      b         = TOther $ UType $ U 2 "b"
      c         = TOther $ UType $ U 3 "c"
      bool      = TCon "Bool" []
      gType     = TOther $ Fn a b
      mcaseType = TOther $ Fn gType $ TOther $ Fn bool b
      fType     = TOther $ Fn mcaseType c
      f_1Type   = TOther $ Fn a mcaseType
      mcase     = MCaseT
        mcaseType
        (NE.singleton
          ( [VarPat gType "g", BoolPat bool True]
          , AppT b (VarT gType "g") (VarT a "y")
          )
        )
      expr     = AppT c (VarT fType "f") mcase
      expected = AppT c
                      (VarT fType "f")
                      (AppT mcaseType (VarT f_1Type "f_1") (VarT a "y"))
      genName               = pure "f_1"
      (result, newBindings) = evalState (liftAllLambdas genName expr) 1
    result `shouldBe` expected
    newBindings
      `shouldBe` [ ( "f_1"
                   , NE.singleton
                     ( [VarPat a "y", VarPat gType "g", BoolPat bool True]
                     , AppT b (VarT gType "g") (VarT a "y")
                     )
                   )
                 ]
  it "lifts an mcase with multiple branches" $ do
    -- f (True 1 -> y; False n -> z) x ==> f (g y z) x
    --                                     g = y z True 1  -> y
    --                                         y z False n -> z
    -- y : a
    -- z : a
    -- x : b
    -- f : (Bool -> Int -> a) -> b -> c
    let
      a         = TOther $ UType $ U 1 "a"
      b         = TOther $ UType $ U 2 "b"
      c         = TOther $ UType $ U 3 "c"
      bool      = TCon "Bool" []
      int       = TCon "Int" []
      mcaseType = TOther $ Fn bool $ TOther $ Fn int a
      fType     = TOther $ Fn mcaseType $ TOther $ Fn b c
      mcase     = MCaseT
        mcaseType
        (  ([BoolPat bool True, IntPat int 1], VarT a "y")
        :| [([BoolPat bool False, VarPat int "n"], VarT a "z")]
        )
      expr =
        AppT c (AppT (TOther (Fn b c)) (VarT fType "f") mcase) (VarT b "x")
      gType    = TOther $ Fn a $ TOther $ Fn a mcaseType
      expected = AppT
        c
        (AppT
          (TOther (Fn b c))
          (VarT fType "f")
          (AppT mcaseType
                (AppT (TOther (Fn a mcaseType)) (VarT gType "g") (VarT a "y"))
                (VarT a "z")
          )
        )
        (VarT b "x")
      (result, newBindings) = evalState (liftAllLambdas (pure "g") expr) 1
    newBindings
      `shouldBe` [ ( "g"
                   , ( [ VarPat a "y"
                       , VarPat a "z"
                       , BoolPat bool True
                       , IntPat int 1
                       ]
                     , VarT a "y"
                     )
                     :| [ ( [ VarPat a "y"
                            , VarPat a "z"
                            , BoolPat bool False
                            , VarPat int "n"
                            ]
                          , VarT a "z"
                          )
                        ]
                   )
                 ]
    result `shouldBe` expected

test_liftLet :: Spec
test_liftLet = describe "liftLet" $ do
  it "lifts a simple let" $ do
    -- f (let x = y in x) ==> let x = y in f x
    let
      a    = TOther $ UType (U 1 "a")
      b    = TOther $ UType (U 2 "b")
      hole = AppT b (VarT (TOther (Fn a b)) "f")
      result =
        liftLet emptyGenName mempty "x" (VarT a "y") (VarT a "x") Nothing hole
      expectedResult = LetT
        b
        [("x", VarT a "y", Nothing)]
        (AppT b (VarT (TOther (Fn a b)) "f") (VarT a "x"))
    result `shouldBe` pure expectedResult
  it "lifts a nested let" $ do
    -- f (let x = y in x) g h ==> let x = y in f x g h
    -- g :: b
    -- h :: a
    -- x :: a
    -- y :: a
    -- f :: a -> b -> a -> b
    let
      a       = TOther $ UType (U 1 "a")
      b       = TOther $ UType (U 2 "b")
      fType   = TOther $ Fn a $ TOther $ Fn b $ TOther $ Fn a b
      fxType  = TOther $ Fn b $ TOther $ Fn a b
      fxgType = TOther $ Fn a b
      hole e = AppT
        b
        (AppT fxgType (AppT fxType (VarT fType "f") e) (VarT b "g"))
        (VarT a "h")
      result =
        liftLet emptyGenName mempty "x" (VarT a "y") (VarT a "x") Nothing hole
      expectedResult = LetT
        b
        [("x", VarT a "y", Nothing)]
        (AppT
          b
          (AppT fxgType (AppT fxType (VarT fType "f") (VarT a "x")) (VarT b "g")
          )
          (VarT a "h")
        )
    result `shouldBe` pure expectedResult
  it "lifts a let that needs renaming" $ do
    -- f (let f = y in f) ==> let g = y in f g
    let
      a       = TOther $ UType (U 1 "a")
      b       = TOther $ UType (U 2 "b")
      hole    = AppT b (VarT (TOther (Fn a b)) "f")
      genName = pure "g" :: Identity Name
      result  = liftLet genName
                        (Set.singleton "f")
                        "f"
                        (VarT a "y")
                        (VarT a "f")
                        Nothing
                        hole
      expectedResult = LetT
        b
        [("g", VarT a "y", Nothing)]
        (AppT b (VarT (TOther (Fn a b)) "f") (VarT a "g"))
    result `shouldBe` pure expectedResult

test_liftLetsFromApplication :: Spec
test_liftLetsFromApplication = describe "liftLetsFromApplication" $ do
  let
  it "lifts a simple let" $ do
    -- f (let x = y in x) ==> let x = y in f x
    let
      expr = AppT ()
                  (VarT () "f")
                  (LetT () [("x", VarT () "y", Nothing)] (VarT () "x"))
      result = withGenNameFromList ["n1", "n2", "n3", "n4"]
        $ \genName -> liftLetsFromApplication genName expr
      expectedResult = LetT ()
                            [("x", VarT () "y", Nothing)]
                            (AppT () (VarT () "f") (VarT () "x"))
    expectedResult `shouldBe` result
  it "lifts lets out of a multi-arg application" $ do
    -- f (let x = y in x) (let z = 1 in z) ==> let x = y, z = 1 in f x
    let expr = AppT
          ()
          (AppT ()
                (VarT () "f")
                (LetT () [("x", VarT () "y", Nothing)] (VarT () "x"))
          )

          (LetT () [("z", IntLitT () 1, Nothing)] (VarT () "z"))
        result = withGenNameFromList ["n1", "n2", "n3", "n4"]
          $ \genName -> liftLetsFromApplication genName expr
        expectedResult = LetT
          ()
          [("x", VarT () "y", Nothing), ("z", IntLitT () 1, Nothing)]
          (AppT () (AppT () (VarT () "f") (VarT () "x")) (VarT () "z"))
    expectedResult `shouldBe` result
  it "lifts lets with clashing names, renaming as necessary (1)" $ do
    -- f x (let x = y in x) (let x = 1 in x) x ==> let n1 = y, n2 = 1 in f n1 n2 x
    let expr = AppT
          ()
          (AppT
            ()
            (AppT ()
                  (AppT () (VarT () "f") (VarT () "x"))
                  (LetT () [("x", VarT () "y", Nothing)] (VarT () "x"))
            )

            (LetT () [("x", IntLitT () 1, Nothing)] (VarT () "x"))
          )
          (VarT () "x")
        result = withGenNameFromList ["n1", "n2"]
          $ \genName -> liftLetsFromApplication genName expr
        expectedResult = LetT
          ()
          [("n1", VarT () "y", Nothing), ("n2", IntLitT () 1, Nothing)]

          (AppT
            ()
            (AppT
              ()
              (AppT () (AppT () (VarT () "f") (VarT () "x")) (VarT () "n1"))
              (VarT () "n2")
            )
            (VarT () "x")
          )
    expectedResult `shouldBe` result
  it "lifts lets with clashing names, renaming as necessary (2)" $ do
    -- f (let x = 1, y = 2 in g x y) (let x = 3, y = 4 in h x y) x y
    -- ==>
    -- let n1 = 1, n2 = 2, n3 = 3, n4 = 4 in f (g n1 n2) (h n3 n4) x y
    let expr = AppT
          ()
          (AppT
            ()
            (AppT
              ()
              (AppT
                ()
                (VarT () "f")
                (LetT
                  ()
                  [("x", IntLitT () 1, Nothing), ("y", IntLitT () 2, Nothing)]
                  (AppT () (AppT () (VarT () "g") (VarT () "x")) (VarT () "y"))
                )
              )

              (LetT
                ()
                [("x", IntLitT () 3, Nothing), ("y", IntLitT () 4, Nothing)]
                (AppT () (AppT () (VarT () "h") (VarT () "x")) (VarT () "y"))
              )
            )
            (VarT () "x")
          )
          (VarT () "y")
        result = withGenNameFromList ["n1", "n2", "n3", "n4"]
          $ \genName -> liftLetsFromApplication genName expr
        expectedResult = LetT
          ()
          [ ("n1", IntLitT () 1, Nothing)
          , ("n2", IntLitT () 2, Nothing)
          , ("n3", IntLitT () 3, Nothing)
          , ("n4", IntLitT () 4, Nothing)
          ]

          (AppT
            ()
            (AppT
              ()
              (AppT
                ()
                (AppT
                  ()
                  (VarT () "f")
                  (AppT () (AppT () (VarT () "g") (VarT () "n1")) (VarT () "n2")
                  )
                )
                (AppT () (AppT () (VarT () "h") (VarT () "n3")) (VarT () "n4"))
              )

              (VarT () "x")
            )
            (VarT () "y")
          )
    expectedResult `shouldBe` result
