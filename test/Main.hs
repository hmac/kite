module Main where

import           Test.Hspec
import qualified Test.Syn.Parse
import qualified Test.Syn.Print
import qualified Test.Syn.RoundTrip
import qualified Test.Constraint
import qualified Test.Constraint.Pattern
import qualified Test.Constraint.Bind
import qualified Test.Constraint.FromSyn
import qualified Test.Constraint.Module

import qualified Test.Type2

import qualified Integration.Typecheck

main :: IO ()
main = hspec $ do
  Test.Syn.Parse.test
  Test.Syn.Print.test
  Test.Constraint.test
  Test.Constraint.Pattern.test
  Test.Constraint.Bind.test
  Test.Constraint.FromSyn.test
  Test.Constraint.Module.test
  Integration.Typecheck.test
  Test.Syn.RoundTrip.test
  Test.Type2.test
