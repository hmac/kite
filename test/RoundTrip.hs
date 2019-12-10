module RoundTrip
  ( test
  )
where

-- This module tests the roundtrip property: parse . print == id

import           Test.Hspec
import           Text.Megaparsec                ( parse )

import           Syntax
import           Parse
import           Print

import           Data.Text.Prettyprint.Doc      ( Doc )
import qualified Hedgehog                      as H
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           HaskellWorks.Hspec.Hedgehog

test :: Spec
test = do
  describe "round trip property" $ do
    it "holds for function declarations" $ require roundtripFun
    it "holds for expressions" $ require roundtripSyn
    it "holds for data declarations" $ require roundtripData
    it "holds for typeclass declarations" $ require roundtripTypeclass
    it "holds for typeclass instances" $ require roundtripInstance
    it "holds for import statements" $ require roundtripImport
    it "holds for modules" $ require roundtripModule

roundtripSyn :: H.Property
roundtripSyn = roundtrip genExpr printExpr pExpr

roundtripFun :: H.Property
roundtripFun = roundtrip genFun printFun pFun

roundtripData :: H.Property
roundtripData = roundtrip genData printData pData

roundtripTypeclass :: H.Property
roundtripTypeclass = roundtrip genTypeclass printTypeclass pTypeclass

roundtripInstance :: H.Property
roundtripInstance = roundtrip genInstance printInstance pInstance

roundtripImport :: H.Property
roundtripImport = roundtrip genImport printImport pImport

roundtripModule :: H.Property
roundtripModule = roundtrip genModule printModule pModule

roundtrip :: (Show a, Eq a) => H.Gen a -> (a -> Doc b) -> Parser a -> H.Property
roundtrip gen printer parser = H.withTests 20 $ H.property $ do
  e <- H.forAll gen
  let printed  = show (printer e)
      reparsed = parse parser "" printed
  H.annotate printed
  r <- H.evalEither reparsed
  H.annotateShow (printer r)
  e H.=== r

-- Hedgehog generators

genModule :: H.Gen (Module Syn)
genModule =
  Module
    <$> genModuleName
    <*> Gen.list (Range.linear 0 5) genImport
    <*> Gen.list (Range.linear 0 5) genModuleItem
    <*> Gen.list (Range.linear 0 10) genDecl
    <*> genMetadata

genMetadata :: H.Gen [(String, String)]
genMetadata = Gen.list (Range.linear 0 5) genKV
 where
  genKV :: H.Gen (String, String)
  genKV =
    (,) <$> genLowerString <*> Gen.string (Range.linear 1 100) Gen.alphaNum

genImport :: H.Gen Import
genImport =
  Import
    <$> Gen.bool
    <*> genModuleName
    <*> Gen.maybe genUpperName
    <*> Gen.list (Range.linear 0 3) genModuleItem

genModuleItem :: H.Gen Name
genModuleItem = Gen.choice [genLowerName, genUpperName]

genDecl :: H.Gen (Decl Syn)
genDecl = Gen.choice
  [ FunDecl <$> genFun
  , DataDecl <$> genData
  , TypeclassDecl <$> genTypeclass
  , TypeclassInst <$> genInstance
  , Comment <$> genComment
  ]

genModuleName :: H.Gen ModuleName
genModuleName = ModuleName <$> Gen.list (Range.linear 1 3) genUpperString

genInstance :: H.Gen (Instance Syn)
genInstance =
  Instance
    <$> genUpperName
    <*> Gen.list (Range.singleton 1) genType
    <*> Gen.list (Range.linear 1 5) genInstanceDef
 where
  genInstanceDef :: H.Gen (Name, [Def Syn])
  genInstanceDef = (,) <$> genLowerName <*> Gen.list (Range.linear 1 3) genDef

genTypeclass :: H.Gen Typeclass
genTypeclass =
  Typeclass
    <$> genUpperName
    <*> Gen.list (Range.linear 1 3) genLowerName
    <*> Gen.list (Range.linear 1 3) typeclassdef
 where
  typeclassdef :: H.Gen (Name, Ty)
  typeclassdef = (,) <$> genLowerName <*> genType

genData :: H.Gen Data
genData =
  Data
    <$> genUpperName
    <*> Gen.list (Range.linear 0 3) genLowerName
    <*> Gen.list (Range.linear 1 3) genDataCon

genDataCon :: H.Gen DataCon
genDataCon = DataCon <$> genUpperName <*> Gen.list (Range.linear 0 3) genType

genFun :: H.Gen (Fun Syn)
genFun =
  Fun
    <$> Gen.list (Range.linear 0 5) genComment
    <*> genLowerName
    <*> genType
    <*> Gen.list (Range.linear 1 5) genDef

genType :: H.Gen Ty
genType = Gen.recursive
  Gen.choice
  [TyCon <$> genUpperName, TyVar <$> genLowerName, TyHole <$> genHoleName]
  [ Gen.subterm2 genType genType (:@:)
  , Gen.subterm2 genType genType fn
  , Gen.subterm genType TyList
  , Gen.subterm2 genType genType (\ty1 ty2 -> TyTuple [ty1, ty2])
  ]

genDef :: H.Gen (Def Syn)
genDef = Def <$> Gen.list (Range.linear 1 5) genPattern <*> genExpr

genExpr :: H.Gen Syn
genExpr = Gen.recursive
  Gen.choice
  [ Var <$> genLowerName
  , Cons <$> genUpperName
  , Hole <$> genHoleName
  , IntLit <$> genInt
  , FloatLit <$> Gen.realFloat (Range.linearFrac (-5) 5)
  ]
  [ Gen.subtermM
    genExpr
    (\e -> Abs <$> Gen.list (Range.linear 1 5) genLowerName <*> pure e)
  , Gen.subterm2 genExpr genExpr App
  , Gen.subtermM2 genExpr
                  genExpr
                  (\e1 e2 -> genBinOp >>= \op -> pure (App (App op e1) e2))
  , Gen.subtermM2 genExpr genExpr (\e1 e2 -> Let <$> genLetBinds e1 <*> pure e2)
  , Gen.subtermM3 genExpr
                  genExpr
                  genExpr
                  (\e1 e2 e3 -> Case e1 <$> genCaseAlts e2 e3)
  , StringLit <$> genComment <*> Gen.list (Range.linear 0 2) genStringInterpPair
  ]

genStringInterpPair :: H.Gen (Syn, String)
genStringInterpPair = (,) <$> genExpr <*> genComment

genBinOp :: H.Gen Syn
genBinOp = Gen.element $ Var <$> binOps

genLetBinds :: Syn -> H.Gen [(Name, Syn)]
genLetBinds e = do
  n <- genLowerName
  pure [(n, e)]

genCaseAlts :: Syn -> Syn -> H.Gen [(Pattern, Syn)]
genCaseAlts e1 e2 = do
  p1 <- genPattern
  p2 <- genPattern
  pure [(p1, e1), (p2, e2)]

genPattern :: H.Gen Pattern
genPattern = Gen.recursive
  Gen.choice
  [VarPat <$> genLowerName, pure WildPat, IntPat <$> genInt]
  [TuplePat <$> Gen.list (Range.linear 2 5) genPattern]

genInt :: H.Gen Int
genInt = Gen.int (Range.linear (-5) 5)

genLowerName :: H.Gen Name
genLowerName =
  let gen = Name <$> genLowerString
  in  Gen.filter (\(Name n) -> n `notElem` keywords) gen

genUpperName :: H.Gen Name
genUpperName =
  let gen = Name <$> genUpperString
  in  Gen.filter (\(Name n) -> n `notElem` keywords) gen

-- A hole name can be any combo or numbers or letters, in any case
-- ?1
-- ?hi
-- ?HI
genHoleName :: H.Gen Name
genHoleName = Name <$> Gen.string (Range.linear 1 5) Gen.alphaNum

genComment :: H.Gen String
genComment = Gen.string (Range.linear 0 80) Gen.ascii

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
