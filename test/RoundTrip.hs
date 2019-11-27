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

import qualified Hedgehog                      as H
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           HaskellWorks.Hspec.Hedgehog

test :: Spec
test = do
  describe "round trip property" $ do
    -- it "prints and reparses without changing" $ property roundtrip
    it "prints and reparses without changing" $ require roundtriph

roundtriph :: H.Property
roundtriph = H.property $ do
  e <- H.forAll genSyn
  let printed  = show (printExpr e)
      reparsed = parse pExpr "" printed
  H.annotate printed
  r <- H.evalEither reparsed
  r H.=== e

roundtrip :: Syn -> Bool
roundtrip e =
  let printed  = show (printExpr e)
      reparsed = parse pExpr "" printed
  in  case reparsed of
        Left  _ -> False
        Right r -> r == e

-- Hedgehog generator for Syn and related types
genSyn :: H.Gen Syn
genSyn = Gen.recursive
  Gen.choice
  [Var <$> genLowerName, Cons <$> genUpperName]
  [ Gen.subtermM
    genSyn
    (\e -> Abs <$> Gen.list (Range.linear 1 5) genLowerName <*> pure e)
  , Gen.subterm2 genSyn genSyn App
  , Gen.subtermM2 genSyn genSyn (\e1 e2 -> Let <$> genLetBinds e1 <*> pure e2)
  , Gen.subtermM genSyn (\e -> Case e <$> genCaseAlts)
  ]

genLetBinds :: Syn -> H.Gen [(Name, Syn)]
genLetBinds e = do
  n <- genLowerName
  pure [(n, e)]

genCaseAlts :: H.Gen [(Pattern, Syn)]
genCaseAlts = Gen.list (Range.linear 1 5) ((,) <$> genPattern <*> genSyn)

genPattern :: H.Gen Pattern
genPattern = Gen.recursive
  Gen.choice
  [VarPat <$> genLowerName, pure WildPat, LitPat <$> genLiteral]
  []

genLiteral :: H.Gen Literal
genLiteral = Gen.choice [LitInt <$> Gen.int (Range.linear (-100) 100)]

genLowerName :: H.Gen Name
genLowerName =
  let gen = Name <$> genLowerString
  in  Gen.filter (\(Name n) -> n `notElem` keywords) gen

genUpperName :: H.Gen Name
genUpperName =
  let gen = Name <$> genUpperString
  in  Gen.filter (\(Name n) -> n `notElem` keywords) gen

genLowerString :: H.Gen String
genLowerString = do
  c  <- Gen.lower
  cs <- Gen.list (Range.linear 0 5) Gen.alphaNum
  pure (c : cs)

genUpperString :: H.Gen String
genUpperString = do
  c  <- Gen.upper
  cs <- Gen.list (Range.linear 0 10) Gen.alphaNum
  pure (c : cs)

-- Arbitrary instances for Syn and related types

genAlphaString :: Gen String
genAlphaString = listOf1 arbitrary `suchThat` all isAlpha

genLowercaseName :: Gen Name
genLowercaseName = Name <$> genAlphaString `suchThat` all isLower

-- Shrink by keeping the first character and shrinking the rest
-- Maintains the invariant of being an upper or lower case name
shrinkString :: String -> [String]
shrinkString []       = []
shrinkString [_     ] = []
shrinkString (c : cs) = map (c :) (genericShrink cs)

shrinkName :: Name -> [Name]
shrinkName (Name n) = map Name (shrinkString n)

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
  shrink (VarPat n)      = VarPat <$> shrinkName n
  shrink WildPat         = []
  shrink (LitPat   p   ) = LitPat <$> genericShrink p
  shrink (TuplePat pats) = TuplePat <$> shrink pats
  shrink (ListPat  pats) = ListPat <$> shrink pats
-- shrink the name, then shrink the patterns, then remove the Cons entirely
  shrink (ConsPat name pats) =
    map (`ConsPat` pats) (shrinkName name)
      ++ (ConsPat name <$> shrink pats)
      ++ pats

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
  shrink (Var  n) = Var <$> shrinkName n
  shrink (Cons n) = Cons <$> shrinkName n
  shrink (Abs vars e) =
    map ((`Abs` e) . shrinkName) vars ++ (Abs vars <$> shrink e) ++ [e]
  shrink (App a b) = map (`App` b) (shrink a) ++ (App a <$> shrink b) ++ [a, b]
  shrink (Let binds e) =
    map (`Let` e) (shrinkLetBindings binds) ++ (Let binds <$> shrink e) ++ [e]
  shrink (Case e [_]) = [e]
  shrink (Case e alts) =
    map (`Case` alts) (shrink e) ++ (Case e <$> shrink alts) ++ [e]
  shrink (TupleLit [a, b]) = [a, b]
  shrink (TupleLit es    ) = (TupleLit <$> shrink es) ++ es
  shrink (ListLit  [e]   ) = [e]
  shrink (ListLit  es    ) = (ListLit <$> shrink es) ++ es
  shrink (Lit      l     ) = Lit <$> shrink l

shrinkLetBindings :: [(Name, Syn)] -> [[(Name, Syn)]]
shrinkLetBindings = shrinkList shrinkBind
 where
  shrinkBind :: (Name, Syn) -> [(Name, Syn)]
  shrinkBind (n, e) = zip (shrinkName n) (shrink e)

listOf2 :: Gen a -> Gen [a]
listOf2 gen = sized $ \n -> do
  k <- choose (2, 2 `max` n)
  vectorOf k gen
