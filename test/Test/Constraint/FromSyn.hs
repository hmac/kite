module Test.Constraint.FromSyn
  ( test
  )
where

import           Test.Hspec
import           Constraint
import qualified Constraint.Expr               as E
import qualified Syntax                        as S
import           Canonical                      ( Name(..) )
import           Constraint.FromSyn

test :: Spec
test = do
  let mx     = TopLevel "SomeModule" "x"
      left   = TopLevel "Data.Either" "Left"
      either = TopLevel "Data.Either" "Either"
      maybe  = TopLevel "Data.Maybe" "Maybe"
  describe "Converting Syn to Constraint.Expr" $ do
    it "x" $ do
      fromSyn (S.Var "x") `shouldBe` E.Var "x"
      fromSyn (S.Var mx) `shouldBe` E.Var mx
    it "Left x" $ do
      fromSyn (S.Cons left) `shouldBe` E.Con left
    it "?x" $ do
      fromSyn (S.Hole "x") `shouldBe` E.Hole "x"
    it "\a b -> x" $ do
      fromSyn (S.Abs ["a", "b"] (S.Var "x"))
        `shouldBe` E.Abs ["a", "b"] (E.Var "x")
    it "f x" $ do
      fromSyn (S.App (S.Var "f") (S.Var "x"))
        `shouldBe` E.App (E.Var "f") (E.Var "x")
    it "let x = SomeModule.x in Left e" $ do
      fromSyn (S.Let [("x", S.Var mx)] (S.App (S.Cons left) (S.Var "e")))
        `shouldBe` E.Let [("x", E.Var mx)] (E.App (E.Con left) (E.Var "e"))
    it "let x : Int; x = mx in Left e" $ do
      fromSyn (S.LetA "x" S.TyInt (S.Var mx) (S.App (S.Cons left) (S.Var "e")))
        `shouldBe` E.LetA "x"
                          (E.Forall [] mempty TInt)
                          (E.Var mx)
                          (E.App (E.Con left) (E.Var "e"))
    it "case s of; p -> e" $ do
      fromSyn (S.Case (S.Var "s") [(S.VarPat "p", S.Var "e")])
        `shouldBe` E.Case (E.Var "s") [E.Alt (S.VarPat "p") (E.Var "e")]
    it "(1, 2)" $ do
      fromSyn (S.TupleLit [S.IntLit 1, S.IntLit 2])
        `shouldBe` E.TupleLit [E.IntLit 1, E.IntLit 2]
    it "[1, 2]" $ do
      fromSyn (S.ListLit [S.IntLit 1, S.IntLit 2])
        `shouldBe` E.ListLit [E.IntLit 1, E.IntLit 2]
    it "\"hi\"" $ do
      fromSyn (S.StringLit "hi " [(S.Var "name", " !")])
        `shouldBe` E.StringLit "hi " [(E.Var "name", " !")]
    it "1" $ do
      fromSyn (S.IntLit 1) `shouldBe` E.IntLit 1
  describe "Converting Ty to Type" $ do
    it "Int -> String" $ do
      tyToType (S.TyFun S.TyInt S.TyString)
        `shouldBe` TCon (TopLevel modPrim "->") [TInt, TString]
    it "Either" $ do
      tyToType (S.TyCon either []) `shouldBe` TCon either []
    it "a" $ do
      tyToType (S.TyVar "a") `shouldBe` TVar (R "a")
    it "(Int, String)" $ do
      tyToType (S.TyTuple [S.TyInt, S.TyString])
        `shouldBe` TCon (TopLevel modPrim "Tuple2") [TInt, TString]
    it "?t" $ do
      tyToType (S.TyHole "t") `shouldBe` THole "t"
    it "Int" $ do
      tyToType S.TyInt `shouldBe` TInt
    it "String" $ do
      tyToType S.TyString `shouldBe` TString
    it "Maybe Int" $ do
      tyToType (S.TyCon maybe [S.TyInt]) `shouldBe` TCon maybe [TInt]
    it "Either Int a" $ do
      tyToType (S.TyCon maybe [S.TyInt, S.TyVar "a"])
        `shouldBe` TCon maybe [TInt, TVar (R "a")]
    it "(a -> b) -> a -> b" $ do
      let synType =
            (S.TyVar "a" `S.fn` S.TyVar "b")
              `S.fn` (S.TyVar "a" `S.fn` S.TyVar "b")
      let expType =
            (TVar (R "a") `fn` TVar (R "b"))
              `fn` (TVar (R "a") `fn` TVar (R "b"))
      tyToType synType `shouldBe` expType
    it "Maybe Int -> Int -> Int" $ do
      let synType = S.TyCon maybe [S.TyInt] `S.fn` (S.TyInt `S.fn` S.TyInt)
      let expType = TCon maybe [TInt] `fn` (TInt `fn` TInt)
      tyToType synType `shouldBe` expType
  describe "Converting Ty to Scheme" $ do
    it "(a -> b) -> a -> b" $ do
      let synType =
            (S.TyVar "a" `S.fn` S.TyVar "b")
              `S.fn` (S.TyVar "a" `S.fn` S.TyVar "b")
      let scheme =
            E.Forall [R "a", R "b"] mempty
              $    (TVar (R "a") `fn` TVar (R "b"))
              `fn` (TVar (R "a") `fn` TVar (R "b"))
      tyToScheme synType `shouldBe` scheme
