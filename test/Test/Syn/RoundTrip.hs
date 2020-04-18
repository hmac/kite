module Test.Syn.RoundTrip where

-- This module tests the roundtrip property: parse . print == id

import           Test.Hspec
import           Text.Megaparsec                ( parse, eof )

import           Syn
import           Syn.Parse
import           Syn.Print

import Util

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String (renderString)

import           Control.Applicative            ( liftA2 )
import           Data.Text.Prettyprint.Doc      ( Doc )
import qualified Hedgehog                      as H
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           HaskellWorks.Hspec.Hedgehog

import Text.Megaparsec (errorBundlePretty)

test :: Spec
test = do
  describe "round trip property" $ do
    it "holds for function declarations" $ require roundtripFun
    it "holds for expressions" $ require roundtripSyn
    it "holds for types" $ require roundtripType
    it "holds for data declarations" $ require roundtripData
    it "holds for import statements" $ require roundtripImport
    it "holds for modules" $ require roundtripModule

roundtripSyn :: H.Property
roundtripSyn = roundtrip genExpr printExpr pExpr

roundtripType :: H.Property
roundtripType = roundtrip genType printType pType

roundtripFun :: H.Property
roundtripFun = roundtrip genFun printFun pFun

roundtripData :: H.Property
roundtripData = roundtrip genData printData pData

roundtripImport :: H.Property
roundtripImport = roundtrip genImport printImport pImport

roundtripModule :: H.Property
roundtripModule = roundtrip genModule printModule pModule

roundtrip :: (Show a, Eq a) => H.Gen a -> (a -> Doc b) -> Parser a -> H.Property
roundtrip gen printer parser = H.withTests 50 $ H.property $ do
  e <- H.forAll gen
  let printed  = renderString (layoutSmart defaultLayoutOptions (printer e))
      reparsed = first errorBundlePretty $ parse (parser <* eof) "" printed
  H.annotate printed
  case reparsed of
    Left err -> do
      H.annotate err
      _ <- H.evalEither reparsed
      pure ()
    Right r -> do
      H.annotateShow (printer r)
      e H.=== r

-- Hedgehog generators

genModule :: H.Gen (Module Syn)
genModule =
  Module
    <$> genModuleName
    <*> Gen.list (Range.linear 0 5) genImport
    <*> Gen.list (Range.linear 0 5) genExport
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
    <*> Gen.list (Range.linear 0 3) genImportItem

genExport :: H.Gen (RawName, [RawName])
genExport = Gen.choice [genFunExport, genDataExport]
 where
  genFunExport = (,) <$> genLowerName <*> pure []
  genDataExport =
    (,) <$> genUpperName <*> Gen.list (Range.linear 0 3) genUpperName

genModuleItem :: H.Gen RawName
genModuleItem = Gen.choice [genUpperName, genLowerName]

genImportItem :: H.Gen ImportItem
genImportItem = Gen.choice [genImportSingle, genImportSome, genImportAll]
 where
  genImportSingle = ImportSingle <$> genModuleItem
  genImportAll    = ImportAll <$> genUpperName
  genImportSome =
    ImportSome <$> genUpperName <*> Gen.list (Range.linear 0 3) genModuleItem

genDecl :: H.Gen (Decl Syn)
genDecl = Gen.choice
  [ FunDecl <$> genFun
  , DataDecl <$> genData
  , Comment <$> genComment
  ]

genModuleName :: H.Gen ModuleName
genModuleName = ModuleName <$> Gen.list (Range.linear 1 3) genUpperString

genData :: H.Gen Data
genData =
  Data
    <$> genUpperName
    <*> Gen.list (Range.linear 0 3) genLowerName
    <*> Gen.list (Range.linear 1 3) genDataCon

genDataCon :: H.Gen DataCon
genDataCon = DataCon <$> genUpperName <*> Gen.list (Range.linear 0 3) genType

genFun :: H.Gen (Fun Syn)
genFun = Gen.choice [funWithType, funWithoutType]
 where
  funWithType =
    Fun []
      <$> genLowerName
      <*> (Just <$> genType)
      <*> Gen.list (Range.linear 1 5) genDef
  funWithoutType =
    Fun []
      <$> genLowerName
      <*> pure Nothing
      <*> Gen.list (Range.linear 1 5) genDef

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
  [ Gen.subterm (Gen.small genType) TyList
  , Gen.subterm2 (Gen.small genType) (Gen.small genType) fn
  , Gen.subtermM2
    (Gen.small genType)
    (Gen.small genType)
    (\ty1 ty2 ->
      TyCon <$> Gen.choice [genUpperName, genLowerName] <*> pure [ty1, ty2]
    )
  , Gen.subterm2 (Gen.small genType) (Gen.small genType) (\ty1 ty2 -> TyTuple [ty1, ty2])
  , Gen.subtermM2 (Gen.small genType) (Gen.small genType) genTyRecord
  ]

genTyRecord :: Type -> Type -> H.Gen Type
genTyRecord t1 t2 = do
  f1 <- genLowerName
  f2 <- genLowerName
  pure $ TyRecord [(f1, t1), (f2, t2)]

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
    (Gen.small genExpr)
    (\e -> Abs <$> Gen.list (Range.linear 1 5) genLowerName <*> pure e)
  , Gen.subterm2 (Gen.small genExpr) (Gen.small genExpr) App
  , Gen.subtermM2 (Gen.small genExpr)
                  (Gen.small genExpr)
                  (\e1 e2 -> genBinOp >>= \op -> pure (App (App op e1) e2))
  , Gen.subtermM2 (Gen.small genExpr) (Gen.small genExpr) (\e1 e2 -> Let <$> genLetBinds e1 <*> pure e2)
  , Gen.subtermM3 (Gen.small genExpr)
                  (Gen.small genExpr)
                  (Gen.small genExpr)
                  (\e1 e2 e3 -> Case e1 <$> genCaseAlts e2 e3)
  , StringLit
  <$> genString (Range.linear 0 10)
  <*> Gen.list (Range.linear 0 2) genStringInterpPair
  , Gen.subtermM2 (Gen.small genExpr) (Gen.small genExpr) genRecord
  , Project <$> (Var <$> genLowerName) <*> genLowerName
  ]

genRecord :: Syn -> Syn -> H.Gen Syn
genRecord e1 e2 = do
  f1 <- genLowerName
  f2 <- genLowerName
  pure (Record [(f1, e1), (f2, e2)])

genStringInterpPair :: H.Gen (Syn, String)
genStringInterpPair = (,) <$> genExpr <*> genString (Range.linear 0 10)

genBinOp :: H.Gen Syn
genBinOp = Gen.element $ Var <$> binOps

genLetBinds :: Syn -> H.Gen [(RawName, Syn)]
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

genLowerName :: H.Gen RawName
genLowerName =
  let gen = Name <$> genLowerString
  in  Gen.filter (\(Name n) -> n `notElem` keywords) gen

genUpperName :: H.Gen RawName
genUpperName =
  let gen = Name <$> genUpperString
  in  Gen.filter (\(Name n) -> n `notElem` keywords) gen

-- A hole name can be any combo or numbers or letters, in any case
-- ?1
-- ?hi
-- ?HI
genHoleName :: H.Gen RawName
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
