-- This module tests the roundtrip property: parse . print == id
module Test.Syn.RoundTrip
  ( properties
  ) where


import           Text.Megaparsec                ( eof )

import           AST
import           Data.Name                      ( PkgModuleName(..) )
import           Data.Name.Gen                  ( genModuleName
                                                , genPkgModuleName
                                                )
import           Syn
import           Syn.Parse
import           Syn.Print

import           Util

import           Data.List                      ( inits )
import qualified Data.List.NonEmpty            as NE
import           Prettyprinter
import           Prettyprinter.Render.String    ( renderString )

import qualified Hedgehog                      as H
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

properties :: H.Group
properties = H.Group "Roundtrip properties" $ mapSnd
  (H.withTests 200)
  [ ("function declarations", H.property roundtripFun)
  , ("expressions"          , H.property roundtripSyn)
  , ("types"                , H.property roundtripType)
  , ("data declarations"    , H.property roundtripData)
  , ("import statements"    , H.property roundtripImport)
  , ("modules"              , H.property roundtripModule)
  ]

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
roundtripImport = roundtrip genImport
                            (printImport "kite-roundtrip-tests")
                            (pImport "kite-roundtrip-tests")

roundtripModule :: H.PropertyT IO ()
roundtripModule = roundtrip (genModule "kite-roundtrip-tests")
                            printModule
                            (pModule "kite-roundtrip-tests")

roundtrip
  :: (Show a, Eq a) => H.Gen a -> (a -> Doc b) -> Parser a -> H.PropertyT IO ()
roundtrip gen printer parser = do
  e <- H.forAll gen
  let printed  = renderString (layoutSmart defaultLayoutOptions (printer e))
      reparsed = parse (parser <* eof) "" printed
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

genModule :: PackageName -> H.Gen Module
genModule pkg =
  Module
    <$> (PkgModuleName pkg <$> genModuleName)
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
genImport = do
  qualified <- Gen.bool
  name      <- genPkgModuleName
  alias     <- Gen.maybe genUpperName
  items     <- Gen.list (Range.linear 0 3) genImportItem
  pure $ Import { importQualified = qualified
                , importName      = name
                , importAlias     = alias
                , importItems     = items
                }

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

genData :: H.Gen Data
genData =
  Data
    <$> genUpperName
    <*> Gen.list (Range.linear 0 3) genLowerName
    <*> Gen.list (Range.linear 1 3) genDataCon

genDataCon :: H.Gen DataCon
genDataCon = DataCon <$> genUpperName <*> Gen.list (Range.linear 0 3) genType

-- We want to generate where clauses containing functions but we don't want Hedgehog to recurse
-- infinitely, generating where clauses in where clauses in where clauses in...
-- So we make sure that any functions generated in a where clause don't themselves have where
-- clauses.
genFun :: H.Gen (Fun Syn)
genFun = genFun' True

genFun' :: Bool -> H.Gen (Fun Syn)
genFun' withWhere =
  let whereClause = if withWhere
        then Gen.list (Range.linear 0 3) (genFun' False)
        else pure []
  in  Fun [] <$> genLowerName <*> (Just <$> genType) <*> genExpr <*> whereClause

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
  ]
  [ Gen.subterm2 (Gen.small genType) (Gen.small genType) fn
  , Gen.subterm2 (Gen.small genType) (Gen.small genType) TyIFun
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
  atomicExprGenerators
  [ Gen.subterm2 (Gen.small genFunExpr) (Gen.small genExpr) App
  , Gen.subtermM2 (Gen.small genExpr)
                  (Gen.small genExpr)
                  (\e1 e2 -> genBinOp >>= \op -> pure (App (App op e1) e2))
  , genLet
  , Gen.subtermM3 (Gen.small genExpr)
                  (Gen.small genExpr)
                  (Gen.small genExpr)
                  (\e1 e2 e3 -> Case e1 <$> genCaseAlts e2 e3)
  , genMCase
  , Gen.subterm3 (Gen.small genExpr) (Gen.small genExpr) (Gen.small genExpr)
    $ \e1 e2 e3 -> TupleLit [e1, e2, e3]
  , Gen.subterm3 (Gen.small genExpr) (Gen.small genExpr) (Gen.small genExpr)
    $ \e1 e2 e3 -> ListLit [e1, e2, e3]
  , StringInterp
  <$> genString (Range.linear 0 10)
  <*> Gen.nonEmpty (Range.linear 1 2) genStringInterpPair
  , StringLit <$> genString (Range.linear 0 10)
  , Gen.subtermM2 (Gen.small genExpr) (Gen.small genExpr) genRecord
  , genRecordProjection
  , Gen.subtermM2 (Gen.small genExpr) (Gen.small genExpr)
    $ \e1 e2 -> FCall <$> (('$' :) <$> genLowerString) <*> pure [e1, e2]
  ]

-- | Generators for 'atomic' expressions: expressions that contains no
-- subexpressions.
atomicExprGenerators :: [H.Gen Syn]
atomicExprGenerators =
  [ genVar
  , Con <$> genUpperName
  , Hole <$> genHoleName
  , IntLit <$> genInt
  , BoolLit <$> Gen.bool
  , CharLit <$> Gen.alphaNum
  , pure UnitLit
  ]

-- Generate an expression which could be on the LHS of an application.
genFunExpr :: H.Gen Syn
genFunExpr = Gen.recursive Gen.choice [genVar, genRecordProjection] [genLet]


genVar :: H.Gen Syn
genVar = Var <$> genLowerName

genRecordProjection :: H.Gen Syn
genRecordProjection = Project <$> (Var <$> genLowerName) <*> genLowerString

genLet :: H.Gen Syn
genLet = Gen.subtermM2 (Gen.small genExpr)
                       (Gen.small genExpr)
                       (\e1 e2 -> Let <$> genLetBinds e1 <*> pure e2)

genMCase :: H.Gen Syn
genMCase = do
  -- For simplicity we current just generate two alts
  -- Each alt must have the same number of patterns
  numPatterns <- Gen.integral $ Range.linear 1 5
  p1          <- Gen.list (Range.singleton numPatterns) genPattern
  p2          <- Gen.list (Range.singleton numPatterns) genPattern
  Gen.subterm2 (Gen.small genExpr) (Gen.small genExpr)
    $ \e1 e2 -> MCase [(p1, e1), (p2, e2)]

shrinkExpr :: Syn -> [Syn]
shrinkExpr = \case
  Var     _            -> []
  Con     _            -> []
  Hole    _            -> []
  IntLit  _            -> []
  BoolLit _            -> []
  UnitLit              -> []
  CharLit _            -> []
  IAbs _     e         -> [e]
  App  _     b         -> [b]
  Let  binds body      -> (Let <$> shrinkList2 binds <*> pure body) <> [body]
  Case e     alts      -> [e] <> map snd alts
  TupleLit  es         -> (TupleLit <$> shrinkList2 es) <> es
  ListLit   es         -> (ListLit <$> shrinkList es) <> es
  StringLit _          -> []
  StringInterp p comps -> case NE.uncons comps of
    (_, Just comps') -> [StringInterp p comps']
    (_, Nothing    ) -> [StringLit p]
  Record fields -> Record <$> shrinkList1 fields
  Project r _   -> [r]
  Ann     e _   -> [e]
  -- We never want to generate empty mcases
  MCase alts    -> MCase <$> shrinkList2 alts
  FCall _ args  -> args

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
  f1 <- genLowerString
  f2 <- genLowerString
  pure (Record [(f1, e1), (f2, e2)])

genStringInterpPair :: H.Gen (Syn, String)
genStringInterpPair = (,) <$> genInterpExpr <*> genString (Range.linear 0 10)

genInterpExpr :: H.Gen Syn
genInterpExpr = Gen.choice atomicExprGenerators

genBinOp :: H.Gen Syn
genBinOp = Var <$> Gen.element binOps

genLetBinds :: Syn -> H.Gen [(RawName, Syn, Maybe Type)]
genLetBinds e = do
  n  <- genLowerName
  ty <- Gen.maybe genType
  pure [(n, e, ty)]

genCaseAlts :: Syn -> Syn -> H.Gen [(Pattern, Syn)]
genCaseAlts e1 e2 = do
  p1 <- genPattern
  p2 <- genPattern
  pure [(p1, e1), (p2, e2)]

genPattern :: H.Gen Pattern
genPattern = Gen.shrink shrinkPattern $ Gen.recursive
  Gen.choice
  [genVarPattern, pure (WildPat ()), IntPat () <$> genInt, pure (UnitPat ())]
  [Gen.small $ TuplePat () <$> Gen.list (Range.linear 2 3) genVarPattern]
  where genVarPattern = VarPat () <$> genLowerName

shrinkPattern :: (Pattern -> [Pattern])
shrinkPattern = \case
  VarPat _ _            -> []
  WildPat _             -> []
  IntPat _ _            -> []
  UnitPat _             -> []
  TuplePat t pats       -> TuplePat t <$> shrinkList2 pats
  ListPat  t pats       -> ListPat t <$> shrinkList pats
  ConsPat t c meta pats -> ConsPat t c meta <$> shrinkList pats
  StringPat _ _         -> []
  CharPat   _ _         -> []
  BoolPat   _ _         -> []

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

genLowerString :: H.Gen String
genLowerString = Gen.filter (`notElem` keywords) $ do
  c  <- Gen.lower
  cs <- Gen.list (Range.linear 0 5) Gen.alphaNum
  pure (c : cs)

genUpperString :: H.Gen String
genUpperString = do
  c  <- Gen.upper
  cs <- Gen.list (Range.linear 0 10) Gen.alphaNum
  pure (c : cs)
