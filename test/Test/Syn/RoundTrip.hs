module Test.Syn.RoundTrip where

-- This module tests the roundtrip property: parse . print == id

import           Test.Hspec
import           Text.Megaparsec                ( parse )

import           Syn
import           Syn.Parse
import           Syn.Print

import           Control.Applicative            ( liftA2 )
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
roundtrip gen printer parser = H.withTests 30 $ H.property $ do
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
  typeclassdef :: H.Gen (Name, Type)
  typeclassdef = (,) <$> genLowerName <*> genType

genData :: H.Gen Data
genData =
  Data
    <$> genUpperName
    <*> Gen.list (Range.linear 0 3) genLowerName
    <*> Gen.list (Range.linear 1 3) genDataCon

genDataCon :: H.Gen DataCon
genDataCon = Gen.choice [genSimpleDataCon, genRecordCon]
 where
  genSimpleDataCon =
    DataCon <$> genUpperName <*> Gen.list (Range.linear 0 3) genType
  genRecordCon = RecordCon <$> genUpperName <*> Gen.list
    (Range.linear 1 3)
    ((,) <$> genLowerName <*> genType)

genFun :: H.Gen (Fun Syn)
genFun =
  Fun
    <$> Gen.list (Range.linear 0 5) genComment
    <*> genLowerName
    <*> genType
    <*> genConstraint
    <*> Gen.list (Range.linear 1 5) genDef

genConstraint :: H.Gen (Maybe Constraint)
genConstraint = Gen.small $ Gen.recursive
  Gen.choice
  [ Just
      <$> (   CInst
          <$> genUpperName
          <*> (map TyVar <$> Gen.list (Range.linear 1 1) genLowerName)
          )
  ]
  [Gen.subterm2 genConstraint genConstraint (liftA2 CTuple)]

-- TyInt and TyString are omitted here because without parsing the whole module
-- we can't distinguish between a locally defined Int type and the builtin Int
-- type - therefore the roundtrip property can't straightforwardly hold for
-- these types.
genType :: H.Gen Type
genType = Gen.recursive
  Gen.choice
  [ TyCon <$> genUpperName <*> pure []
  , TyVar <$> genLowerName
  , TyHole <$> genHoleName
  ]
  [ Gen.subterm genType TyList
  , Gen.subterm2 genType genType fn
  , Gen.subtermM2
    genType
    genType
    (\ty1 ty2 ->
      TyCon <$> Gen.choice [genUpperName, genLowerName] <*> pure [ty1, ty2]
    )
  , Gen.subterm2 genType genType (\ty1 ty2 -> TyTuple [ty1, ty2])
  ]

genDef :: H.Gen (Def Syn)
genDef = Def <$> Gen.list (Range.linear 1 5) genPattern <*> genExpr

genExpr :: H.Gen Syn
genExpr = Gen.recursive
  Gen.choice
  [ Var <$> genLowerName
  , Con <$> genUpperName
  , Hole <$> genHoleName
  , IntLit <$> genInt
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
  , StringLit
  <$> genString (Range.linear 0 10)
  <*> Gen.list (Range.linear 0 2) genStringInterpPair
  ]

genStringInterpPair :: H.Gen (Syn, String)
genStringInterpPair = (,) <$> genExpr <*> genString (Range.linear 0 10)

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
genComment = Gen.string (Range.linear 1 10) (Gen.filter (/= '\n') Gen.ascii)

-- Generates a string with valid escape sequences - i.e. a backslash must be
-- followed by another backslash or a double quote.
genString :: Range.Range Int -> H.Gen String
genString range = Gen.recursive
  Gen.choice
  [genEscape, genStringWithoutEscapes]
  [Gen.subterm2 (genString range) (genString range) (<>)]
 where
  genStringWithoutEscapes :: H.Gen String
  genStringWithoutEscapes = Gen.string range (Gen.filter (/= '\\') Gen.ascii)
  genEscape :: H.Gen String
  genEscape = Gen.choice [pure "\\\\", pure "\\\""]

-- A simplified generator to speed up generation and shrinking
genLowerString :: H.Gen String
genLowerString = Gen.element ["a", "b", "c", "foo", "bar", "xs"]

genLowerString' :: H.Gen String
genLowerString' = do
  c  <- Gen.lower
  cs <- Gen.list (Range.linear 0 5) Gen.alphaNum
  pure (c : cs)

genUpperString :: H.Gen String
genUpperString = Gen.element ["A", "B", "C", "Foo", "Bar"]

genUpperString' :: H.Gen String
genUpperString' = do
  c  <- Gen.upper
  cs <- Gen.list (Range.linear 0 10) Gen.alphaNum
  pure (c : cs)
