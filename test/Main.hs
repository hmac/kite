module Main where

import           Control.Monad                  ( void )

import           Test.Hspec
import qualified Test.Syn.Parse
import qualified Test.Syn.Print
import qualified Test.Syn.RoundTrip
import qualified Test.Constraint
import qualified Test.Constraint.Pattern
import qualified Test.Constraint.Bind
import qualified Test.Constraint.FromSyn
import qualified Test.Constraint.Module
import qualified Test.Type

import qualified Integration.Typecheck

import           Hedgehog
import           Test.Hspec.Hedgehog            ( hedgehog )
import qualified Type                           ( tests )

main :: IO ()
main = do
  hspec $ do
    Test.Syn.Parse.test
    Test.Syn.Print.test
    Test.Constraint.test
    Test.Constraint.Pattern.test
    Test.Constraint.Bind.test
    Test.Constraint.FromSyn.test
    Test.Constraint.Module.test
    Test.Type.test
    Integration.Typecheck.test
    Test.Syn.RoundTrip.test
    describe "Type" $ it "passes all Hedgehog tests" $ do
      success <- checkParallel Type.tests
      success `shouldBe` True
