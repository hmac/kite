module Test.Type2
  ( test
  )
where

import           Test.Hspec
import           Constraint.Expr
import           Constraint
import           Constraint.Print

import           Type2

test :: Spec
test = do
  describe "Type2" $ do
    it "infers record types" $ do
      let expr = Record [("five", IntLit 5), ("msg", StringLit "Hello" [])]
      infersType expr (TRecord [("five", TInt), ("msg", TString)])
    it "infers record projection types" $ do
      let record = Record [("five", IntLit 5), ("msg", StringLit "Hello" [])]
      let expr   = Project record "five"
      infersType expr TInt

infersType :: Exp -> Type -> Expectation
infersType expr expectedType = case infer mempty expr of
  Left  err -> expectationFailure $ show (printError err)
  Right ty  -> ty `shouldBe` expectedType
