module ConstraintTest
  ( test
  )
where

import           Test.Hspec
import qualified Data.Map.Strict               as Map
import           Constraint
import           Constraint.Solve               ( solve )
import           Constraint.Generate            ( run
                                                , generate
                                                , Exp(..)
                                                , Alt(..)
                                                , Con(..)
                                                , Scheme(..)
                                                )

-- Tests the constraint solver

test :: Spec
test = do
  describe "Constraint Monoid" $ do
    it "mempty is the left and right identity for <>" $ do
      let c = Simple $ TCon "A" [] :~: TCon "B" []
      (mempty <> mempty) `shouldBe` Simple CNil
      (c <> mempty) `shouldBe` c
      (mempty <> c) `shouldBe` c
    it "<> is associative" $ do
      let c = Simple $ TCon "A" [] :~: TCon "B" []
      let d = Simple $ TVar (U "0") :~: TVar (U "1")
      let e = Simple $ CNil :^: (TVar (U "0") :~: TVar (U "1"))
      c <> (d <> e) `shouldBe` (c <> d) <> e
  describe "Constraint solving" $ do
    it "solves a simple constraint" $ do
      let c1 = TVar (U "a") :~: TCon "List" [TVar (U "b")]
      let c2 = TVar (U "a") :~: TCon "List" [TCon "Bool" []]
      solve [c1, c2] `shouldBe` Right
        [ TVar (U "b") :~: TCon "Bool" []
        , TVar (U "a") :~: TCon "List" [TCon "Bool" []]
        ]
  describe "Constraint generation" $ do
    it "unknown variables generate fresh unification vars" $ do
      run (generate mempty (Var "foo")) `shouldBe` (TVar (U "0"), Simple CNil)
    it "simple lets" $ do
      let env  = Map.singleton "True" (Forall [] CNil (TCon "Bool" []))
      let expr = Let "x" (Con (C "True")) (Var "x")
      run (generate env expr) `shouldBe` (TCon "Bool" [], Simple CNil)
    it "compound lets" $ do
      -- let x = True
      --     id y = y
      --  in id x
      let env = Map.singleton "True" (Forall [] CNil (TCon "Bool" []))
      let expr = Let
            "x"
            (Con (C "True"))
            (Let "id" (Abs "y" (Var "y")) (App (Var "id") (Var "x")))
      run (generate env expr)
        `shouldBe` ( TVar (U "1")
                   , Simple
                     (   TCon "->" [TVar (U "0"), TVar (U "0")]
                     :~: TCon "->" [TCon "Bool" [], TVar (U "1")]
                     )
                   )
  describe "Constraint generate and solving combined" $ do
    it "solves simple function applications" $ do
      let env = Map.singleton "True" (Forall [] CNil (TCon "Bool" []))
      let expr          = App (Abs "x" (Var "x")) (Con (C "True"))
      let (_, Simple c) = run (generate env expr)
      solve [c] `shouldBe` Right
        [TVar (U "0") :~: TCon "Bool" [], TVar (U "1") :~: TCon "Bool" []]
    it "solves compound lets" $ do
      let env = Map.singleton "True" (Forall [] CNil (TCon "Bool" []))
      let expr = Let
            "x"
            (Con (C "True"))
            (Let "id" (Abs "y" (Var "y")) (App (Var "id") (Var "x")))
      let (_, Simple c) = run (generate env expr)
      solve [c] `shouldBe` Right
        [TVar (U "0") :~: TCon "Bool" [], TVar (U "1") :~: TCon "Bool" []]
    it "solves simple case expressions" $ do
      let env = Map.fromList
            [ ("True" , Forall [] CNil (TCon "Bool" []))
            , ("False", Forall [] CNil (TCon "Bool" []))
            ]
      let expr = Case
            (Con (C "True"))
            [ Alt (C "True")  [] (Con (C "False"))
            , Alt (C "False") [] (Con (C "True"))
            ]
      let (t, c) = run (generate env expr)
      let expectedConstraints =
            [ TCon "Bool" [] :~: TCon "Bool" []
            , TCon "Bool" [] :~: TVar (U "0")
            , TCon "Bool" [] :~: TVar (U "0")
            ]
      t `shouldBe` TVar (U "0")
      c `shouldBe` mconcat (map Simple expectedConstraints)
      solve expectedConstraints
        `shouldBe` Right [TVar (U "0") :~: TCon "Bool" []]
