module ConstraintTest
  ( test
  )
where

import           Test.Hspec
import           Constraint

-- Tests the constraint solver

test :: Spec
test = do
  describe "Constraint solving" $ do
    it "solves a simple constraint" $ do
      let c1 = TVar (U "a") :~: TCon "List" [TVar (U "b")]
      let c2 = TVar (U "a") :~: TCon "List" [TCon "Bool" []]
      solve [c1, c2] `shouldBe` Right
        [ TVar (U "b") :~: TCon "Bool" []
        , TVar (U "a") :~: TCon "List" [TVar (U "b")]
        ]
