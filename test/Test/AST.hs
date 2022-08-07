module Test.AST (test) where

import AST (ExprT (..), Pat (..), fv)
import qualified Data.Map as Map
import Test.Hspec

test :: Spec
test = parallel $ do
  test_fv

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
  it "an expression with one bound variable and one free variable, with the same name" $ do
    let e :: ExprT String ()
        e = IAppT () (VarT () "f") (IAbsT () (VarPat () "f") () (VarT () "f"))
    fv e `shouldBe` Map.fromList [("f", ())]
  it "an expression with two bound variables and one free variable" $ do
    let e :: ExprT String ()
        e = IAbsT () (VarPat () "x") () (IAbsT () (VarPat () "y") () (AppT () (VarT () "f") (VarT () "x")))
    fv e `shouldBe` Map.fromList [("f", ())]
