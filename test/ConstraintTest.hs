module ConstraintTest
  ( test
  )
where

import           Test.Hspec
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
      run (generate [] (Var "foo")) `shouldBe` (TVar (U "0"), Simple CNil)
  describe "Constraint generate and solving combined" $ do
    it "solves simple function applications" $ do
      let env  = [("True", Forall [] CNil (TCon "Bool" []))]
      let expr = App (Abs "x" (Var "x")) (Con (C "True"))
      let (_, Simple CNil :^^: (Simple CNil :^^: Simple c)) =
            run (generate env expr)
      solve [c] `shouldBe` Right
        [TVar (U "0") :~: TCon "Bool" [], TVar (U "1") :~: TCon "Bool" []]
