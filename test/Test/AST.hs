module Test.AST
  ( test
  ) where

import           AST                            ( ExprT(..)
                                                , Pat(..)
                                                , fv
                                                , fvPat
                                                )
import qualified Data.Map                      as Map
import           Test.Hspec

test :: Spec
test = parallel $ do
  test_fv
  test_fvPat

test_fv :: Spec
test_fv = describe "fv" $ do
  it "an expression with no free variables" $ do
    let e :: ExprT String ()
        e = IntLitT () 1
    fv e `shouldBe` mempty
  it "an expression with a single free variable" $ do
    let e :: ExprT String ()
        e = VarT () "x"
    fv e `shouldBe` Map.singleton "x" ()
  it "an expression with two free variables" $ do
    let e :: ExprT String ()
        e = AppT () (VarT () "f") (VarT () "x")
    fv e `shouldBe` Map.fromList [("f", ()), ("x", ())]
  it "an expression with duplicate variables" $ do
    let e :: ExprT String ()
        e = AppT () (AppT () (VarT () "f") (VarT () "x")) (VarT () "x")
    fv e `shouldBe` Map.fromList [("f", ()), ("x", ())]
  it "an expression with a bound variable" $ do
    let e :: ExprT String ()
        e = IAbsT () (VarPat () "x") () (VarT () "x")
    fv e `shouldBe` mempty
  it "an expression with one bound variable and one free variable" $ do
    let e :: ExprT String ()
        e = IAppT () (VarT () "f") (IAbsT () (VarPat () "x") () (VarT () "x"))
    fv e `shouldBe` Map.fromList [("f", ())]
  it
      "an expression with one bound variable and one free variable, with the same name"
    $ do
        let e :: ExprT String ()
            e =
              IAppT () (VarT () "f") (IAbsT () (VarPat () "f") () (VarT () "f"))
        fv e `shouldBe` Map.fromList [("f", ())]
  it "an expression with two bound variables and one free variable" $ do
    let e :: ExprT String ()
        e = IAbsT
          ()
          (VarPat () "x")
          ()
          (IAbsT () (VarPat () "y") () (AppT () (VarT () "f") (VarT () "x")))
    fv e `shouldBe` Map.fromList [("f", ())]

test_fvPat :: Spec
test_fvPat = describe "fvPat" $ do
  it "a pattern with no variables" $ do
    let e :: Pat () String
        e = IntPat () 1
    fvPat e `shouldBe` mempty
  it "a single variable pattern" $ do
    let e :: Pat () String
        e = VarPat () "x"
    fvPat e `shouldBe` [("x", ())]
  it "a pair pattern" $ do
    let e :: Pat Int String
        e = TuplePat 3 [VarPat 1 "x", VarPat 2 "y"]
    fvPat e `shouldBe` [("x", 1), ("y", 2)]
  it "a complex nested pattern" $ do
    -- ([C c, D, x], (y, [z, (w, v)], "hi"))
    let e :: Pat Int String
        e = TuplePat
          1
          [ ListPat
            2
            [ ConsPat 3 "C" Nothing [VarPat 4 "c"]
            , ConsPat 5 "D" Nothing []
            , VarPat 6 "x"
            ]
          , TuplePat
            7
            [ VarPat 8 "y"
            , ListPat
              9
              [VarPat 10 "z", TuplePat 11 [VarPat 12 "w", VarPat 13 "v"]]
            , StringPat 14 "hi"
            ]
          ]
    fvPat e
      `shouldBe` [("c", 4), ("x", 6), ("y", 8), ("z", 10), ("w", 12), ("v", 13)]
