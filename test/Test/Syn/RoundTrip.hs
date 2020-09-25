module Test.Syn.RoundTrip where

-- This module tests the roundtrip property: parse . print == id

import           Test.Hspec
import           Text.Megaparsec                ( parse
                                                , eof
                                                , errorBundlePretty
                                                )

import           Syn
import           Syn.Parse
import           Syn.Print

import           Util

import           Data.List                      ( inits )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String
                                                ( renderString )

import qualified Hedgehog                      as H
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Hspec.Hedgehog     hiding ( Var )

test :: Spec
test = describe "round trip property" $ modifyMaxSuccess (const 200) $ do
  it "holds for function declarations" roundtripFun
  it "holds for expressions"           roundtripSyn
  it "holds for types"                 roundtripType
  it "holds for data declarations"     roundtripData
  it "holds for import statements"     roundtripImport
  it "holds for modules"               roundtripModule

roundtripSyn :: H.PropertyT IO ()
roundtripSyn = roundtrip genExpr printExpr pExpr

roundtripType :: H.PropertyT IO ()
roundtripType = roundtrip genType printType pType

-- TODO: roundtrip patterns

roundtripFun :: H.PropertyT IO ()
roundtripFun = roundtrip genFun printFun pFun

roundtripData :: H.PropertyT IO ()
roundtripData = roundtrip genData printData pData

roundtripImport :: H.PropertyT IO ()
roundtripImport = roundtrip genImport printImport pImport

roundtripModule :: H.PropertyT IO ()
roundtripModule = roundtrip genModule printModule pModule

roundtrip
  :: (Show a, Eq a) => H.Gen a -> (a -> Doc b) -> Parser a -> H.PropertyT IO ()
roundtrip gen printer parser = hedgehog $ do
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

genModule :: H.Gen Module
genModule =
  Module
    <$> genModuleName
    <*> Gen.list (Range.linear 0 5) genImport
    <*> Gen.list (Range.linear 0 5) genExport
    <*> (uniqueFunNames <$> Gen.list (Range.linear 0 10) genDecl)
    <*> genMetadata

-- Filter a list of decls, removing any functions with duplicate names
uniqueFunNames :: [Decl Syn] -> [Decl Syn]
uniqueFunNames = go []
 where
  go _ [] = []
  go seen (FunDecl f : rest)
    | funName f `elem` seen = go seen rest
    | otherwise             = FunDecl f : go (funName f : seen) rest
  go seen (notFun : rest) = notFun : go seen rest


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
genDecl =
  Gen.choice [FunDecl <$> genFun, DataDecl <$> genData, Comment <$> genComment]

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
genFun = Fun [] <$> genLowerName <*> (Just <$> genType) <*> genExpr

genType :: H.Gen Type
genType = Gen.recursive
  Gen.choice
  [ pure TyString
  , pure TyChar
  , pure TyInt
  , pure TyBool
  , pure TyList
  , pure TyUnit
  , TyCon <$> genUpperName
  , TyVar <$> genLowerName
  , TyHole <$> genHoleName
  ]
  [ Gen.subterm2 (Gen.small genType) (Gen.small genType) fn
  , Gen.subterm2 (Gen.small genType) (Gen.small genType) TyApp
  , Gen.subterm2 (Gen.small genType)
                 (Gen.small genType)
                 (\ty1 ty2 -> TyTuple [ty1, ty2])
  , Gen.subtermM2 (Gen.small genType) (Gen.small genType) genTyRecord
  ]

genTyRecord :: Type -> Type -> H.Gen Type
genTyRecord t1 t2 = do
  f1 <- genLowerName
  f2 <- genLowerName
  pure $ TyRecord [(f1, t1), (f2, t2)]

genExpr :: H.Gen Syn
genExpr = Gen.shrink shrinkExpr $ Gen.recursive
  Gen.choice
  [ genVar
  , Con <$> genUpperName
  , Hole <$> genHoleName
  , IntLit <$> genInt
  , BoolLit <$> Gen.bool
  , CharLit <$> Gen.unicode
  , pure UnitLit
  ]
  [ genAbs
  , Gen.subterm2 (Gen.small genFunExpr) (Gen.small genExpr) App
  , Gen.subtermM2 (Gen.small genExpr)
                  (Gen.small genExpr)
                  (\e1 e2 -> genBinOp >>= \op -> pure (App (App op e1) e2))
  , genLet
  , Gen.subtermM3 (Gen.small genExpr)
                  (Gen.small genExpr)
                  (Gen.small genExpr)
                  (\e1 e2 e3 -> Case e1 <$> genCaseAlts e2 e3)
  , StringInterp
  <$> genString (Range.linear 0 10)
  <*> Gen.list (Range.linear 1 2) genStringInterpPair
  , StringLit <$> genString (Range.linear 0 10)
  , Gen.subtermM2 (Gen.small genExpr) (Gen.small genExpr) genRecord
  , genRecordProjection
  ]

-- Generate an expression which could be on the LHS of an application.
genFunExpr :: H.Gen Syn
genFunExpr =
  Gen.recursive Gen.choice [genVar, genRecordProjection] [genAbs, genLet]

genAbs :: H.Gen Syn
genAbs = Gen.subtermM
  (Gen.small genExpr)
  (\e -> Abs <$> Gen.list (Range.linear 1 5) genLowerName <*> pure e)

genVar :: H.Gen Syn
genVar = Var <$> genLowerName

genRecordProjection :: H.Gen Syn
genRecordProjection = Project <$> (Var <$> genLowerName) <*> genLowerName

genLet :: H.Gen Syn
genLet = Gen.subtermM2 (Gen.small genExpr)
                       (Gen.small genExpr)
                       (\e1 e2 -> Let <$> genLetBinds e1 <*> pure e2)

shrinkExpr :: Syn -> [Syn]
shrinkExpr = \case
  Var     _            -> []
  Con     _            -> []
  Hole    _            -> []
  IntLit  _            -> []
  BoolLit _            -> []
  UnitLit              -> []
  Abs (v : vs) e       -> fmap (\vars -> Abs (v : vars) e) (shrinkList1 vs)
  Abs _        e       -> [e]
  App _        b       -> [b]
  LetA _n _sch _e body -> [body]
  Let  _binds body     -> [body]
  Case e      alts     -> [e] <> map snd alts
  TupleLit  es         -> (TupleLit <$> shrinkList1 es) <> es
  ListLit   es         -> (ListLit <$> shrinkList es) <> es
  StringLit s          -> []
  StringInterp p _     -> [StringInterp p []]
  Record fields        -> Record <$> shrinkList1 fields
  Project r _          -> [r]

shrinkList :: [a] -> [[a]]
shrinkList = tail . reverse . inits

-- | Shrinks a list to a minimum of 1 element
shrinkList1 :: [a] -> [[a]]
shrinkList1 = filter (not . null) . shrinkList

-- | Shrinks a list to a minimum of 2 elements
shrinkList2 :: [a] -> [[a]]
shrinkList2 = filter ((> 1) . length) . shrinkList

genRecord :: Syn -> Syn -> H.Gen Syn
genRecord e1 e2 = do
  f1 <- genLowerName
  f2 <- genLowerName
  pure (Record [(f1, e1), (f2, e2)])

genStringInterpPair :: H.Gen (Syn, String)
genStringInterpPair = (,) <$> genExpr <*> genString (Range.linear 0 10)

genBinOp :: H.Gen Syn
genBinOp = Var <$> Gen.element binOps

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
genPattern = Gen.shrink shrinkPattern $ Gen.recursive
  Gen.choice
  [genVarPattern, pure WildPat, IntPat <$> genInt, pure UnitPat]
  [Gen.small $ TuplePat <$> Gen.list (Range.linear 2 3) genVarPattern]
  where genVarPattern = VarPat <$> genLowerName

shrinkPattern :: (Pattern -> [Pattern])
shrinkPattern = \case
  VarPat _       -> []
  WildPat        -> []
  IntPat _       -> []
  UnitPat        -> []
  TuplePat pats  -> TuplePat <$> shrinkList2 pats
  ListPat  pats  -> ListPat <$> shrinkList pats
  ConsPat c pats -> ConsPat c <$> shrinkList pats
  StringPat _    -> []

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

-- A hole name can be any combo of numbers or letters
-- ?1
-- ?hi
-- ?HI
genHoleName :: H.Gen RawName
genHoleName = Name <$> Gen.filter
  (`notElem` keywords)
  (Gen.string (Range.linear 1 5) Gen.alphaNum)

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
