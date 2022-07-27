module Main where

import qualified Test.Syn.Parse
import qualified Test.Syn.Print
import qualified Test.Syn.RoundTrip
import qualified Test.Type
import qualified Test.Type.Module

import           Test.Tasty
import           Test.Tasty.Hedgehog           as HH
import           Test.Tasty.Hspec              as HS

import qualified Integration.Parse
import qualified Integration.Typecheck

import qualified Type                           ( test )

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = localOption TreatPendingAsSuccess . testGroup "Tests" <$> sequence
  [ HS.testSpec "parsing tests" Test.Syn.Parse.test
  , HS.testSpec "printing tests" Test.Syn.Print.test
  , HS.testSpec "typing tests" Test.Type.test
  , HS.testSpec "module typing tests" Test.Type.Module.test
  , HS.testSpec "typechecking integration tests" Integration.Typecheck.test
  , HS.testSpec "parsing integration tests" Integration.Parse.test
  , pure typecheckProperties
  -- , pure roundTripProperties
  ]

roundTripProperties :: TestTree
roundTripProperties = fromGroup Test.Syn.RoundTrip.properties

typecheckProperties :: TestTree
typecheckProperties = fromGroup Type.test
