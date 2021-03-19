module Main where

import           Test.Hspec
import qualified Test.Syn.Parse
import qualified Test.Syn.Print
import qualified Test.Syn.RoundTrip
import qualified Test.Type
import qualified Test.Type.Module

import qualified Integration.Parse
import qualified Integration.Typecheck

import           Hedgehog
import qualified Type                           ( tests )

main :: IO ()
main = do
  hspec $ do
    Test.Syn.Parse.test
    Test.Syn.Print.test
    Test.Type.test
    Test.Type.Module.test
    Integration.Typecheck.test
    Integration.Parse.test
    Test.Syn.RoundTrip.test
    describe "Type" $ it "passes all Hedgehog tests" $ do
      succeeded <- checkParallel Type.tests
      succeeded `shouldBe` True
