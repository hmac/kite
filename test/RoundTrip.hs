{-# LANGUAGE OverloadedStrings #-}
module RoundTrip
  ( test
  )
where

-- This module tests the roundtrip property: parse . print == id

import           Test.Hspec
import           Test.QuickCheck
import           Text.Megaparsec                ( parse )
import           Data.Char                      ( isAlpha
                                                , isUpper
                                                , isLower
                                                )

import           Syntax
import           Parse
import           Print

test :: Spec
test = do
  describe "round trip property"
    $ it "prints and reparses without changing"
    $ property roundtrip

roundtrip :: Syn -> Bool
roundtrip e =
  let printed  = show (printExpr e)
      reparsed = parse pExpr "" printed
  in  case reparsed of
        Left  _ -> False
        Right r -> r == e


-- Arbitrary instances for Syn and related types

genAlphaString :: Gen String
genAlphaString = listOf1 arbitrary `suchThat` all isAlpha

genLowercaseName :: Gen Name
genLowercaseName = Name <$> genAlphaString `suchThat` all isLower

genUppercaseName :: Gen Name
genUppercaseName = Name <$> genAlphaString `suchThat` (isUpper . head)

instance Arbitrary Literal where
  arbitrary = oneof
    [LitInt <$> arbitrary, LitFloat <$> arbitrary, LitString <$> arbitrary]
  shrink = genericShrink

instance Arbitrary Pattern where
  arbitrary = oneof
    [ VarPat <$> genLowercaseName
    , pure WildPat
    , LitPat <$> arbitrary
    , TuplePat <$> listOf2 arbitrary
    , ListPat <$> listOf arbitrary
    , ConsPat <$> genUppercaseName <*> listOf arbitrary
    ]
  shrink = shrinkNothing

instance Arbitrary Syn where
  arbitrary = oneof
    [ Var <$> genLowercaseName
    , Cons <$> genUppercaseName
    , Abs <$> listOf1 genLowercaseName <*> arbitrary
    , App <$> arbitrary <*> arbitrary
    , Let <$> listOf1 ((,) <$> genLowercaseName <*> arbitrary) <*> arbitrary
    , Case <$> arbitrary <*> listOf1 arbitrary
    , TupleLit <$> listOf2 arbitrary
    , ListLit <$> listOf1 arbitrary
    , Lit <$> arbitrary
    ]
-- We need a custom shrink to maintain various nonempty invariants (e.g. on
-- case alternatives)
  shrink = shrinkNothing
  -- shrink (Case e []) = [e]
  -- shrink e           = genericShrink e

listOf2 :: Gen a -> Gen [a]
listOf2 gen = sized $ \n -> do
  k <- choose (2, 2 `max` n)
  vectorOf k gen
