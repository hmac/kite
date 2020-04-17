module Syn.Parse where

import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import           Data.Void                      ( Void )
import           Data.Functor                   ( void )
import           Control.Monad                  ( guard )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Syn

type Parser = Parsec Void String

-- TODO: markdown in comments & doctests
-- TODO: escape quote chars in string literals
-- TODO: heredocs
-- TODO: do notation
-- TODO: where clause
-- TODO: record construction, record patterns
-- TODO: record field syntax
-- TODO: infix constructors (like List ::)
-- TODO: empty data types (e.g. Void)
-- TODO: rename data to type, type to alias?
-- TODO: include package in imports: from std import Data.Either

parseLamFile :: String -> Either String (Module Syn)
parseLamFile input = case parse (pModule <* eof) "" input of
  Left  e -> Left (errorBundlePretty e)
  Right e -> Right e

pModule :: Parser (Module Syn)
pModule = do
  metadata <- optional pMetadata
  void $ symbol "module"
  name    <- lexemeN pModuleName
  exports <- optional . lexemeN . parens $ lexemeN pExport `sepBy` comma
  imports <- many (lexemeN pImport)
  decls   <- many (lexemeN pDecl)
  pure $ Module { moduleName     = name
                , moduleImports  = imports
                , moduleExports  = fromMaybe [] exports
                , moduleDecls    = decls
                , moduleMetadata = fromMaybe [] metadata
                }

pExport :: Parser (RawName, [RawName])
pExport = do
  export     <- pName
  subexports <- fromMaybe [] <$> optional (parens (pName `sepBy` comma))
  pure (export, subexports)

-- ---
-- key1: val1
-- key2: 2
-- ---
pMetadata :: Parser [(String, String)]
pMetadata = do
  void $ string "---" >> newline
  items <- many pMetaItem
  void $ string "---" >> newline
  pure items
 where
  pMetaItem :: Parser (String, String)
  pMetaItem = do
    key <- lowercaseString
    void (symbol ":")
    val <- many alphaNumChar
    void newline
    pure (key, val)

-- import Bar
-- import qualified Baz as Boo (fun1, fun2)
-- import Foo (SomeType(..), OtherType(AConstructor), SomeClass)
--
-- When we have packaging: from some_pkg import ...
pImport :: Parser Import
pImport = do
  void $ symbol "import"
  qualified <- isJust <$> optional (symbol "qualified")
  name      <- pModuleName
  alias     <- optional (symbol "as" >> uppercaseName)
  items     <- optional $ parens (pImportItem `sepBy` comma)
  pure Import { importQualified = qualified
              , importName      = name
              , importAlias     = alias
              , importItems     = fromMaybe [] items
              }

pImportItem :: Parser ImportItem
pImportItem = try pImportAll <|> try pImportSome <|> pImportSingle
 where
  -- Foo(..)
  pImportAll = do
    name <- uppercaseName
    void $ parens (symbol "..")
    pure $ ImportAll name
  -- Foo(Bar, Baz)
  -- Monoid(empty)
  pImportSome = do
    name     <- uppercaseName
    subItems <- parens (pName `sepBy` comma)
    pure $ ImportSome name subItems
  -- Foo
  -- foo
  pImportSingle = ImportSingle <$> pName

-- We ensure that comments are parsed last so that they get attached to a
-- function if directly above one.
-- We want comments above functions to be associated with them, but doing this
-- in the parser leads to some backtracking that worsens error messages and
-- reduces performance, so we keep it simple here. In a later stage of the
-- compiler we merge adjacent comment and function declarations.
pDecl :: Parser (Decl Syn)
pDecl =
  Comment
    <$> pComment
    <|> DataDecl
    <$> pData
    <|> FunDecl
    <$> pFun

pData :: Parser Data
pData = do
  void (symbol "type")
  name   <- uppercaseName
  tyvars <- many lowercaseName
  void (symbolN "=")
  constructors <- lexemeN pCon `sepBy` symbolN "|"
  pure Data { dataName = name, dataTyVars = tyvars, dataCons = constructors }
 where
  pCon :: Parser DataCon
  pCon  = DataCon <$> uppercaseName <*> many pConType

-- TODO: we can make this more flexible by allowing annotations separate from
-- definitions
pFun :: Parser (Fun Syn)
pFun = do
  comments <- many pComment
  name     <- lowercaseName <?> "declaration type name"
  sig      <- optional (symbol ":" >> lexemeN pType)
  defs     <- case sig of
    Just _  -> many (lexemeN (pDef name))
    Nothing -> do
      first <- lexemeN $ do
        bindings <- many pPattern <?> "pattern"
        void (symbolN "=")
        expr <- pExpr
        pure Def { defArgs = bindings, defExpr = expr }
      rest <- many (lexemeN (pDef name))
      pure (first : rest)
  pure Fun { funComments   = comments
           , funName       = name
           , funType       = sig
           , funDefs       = defs
           }

pDef :: RawName -> Parser (Def Syn)
pDef (Name name) = do
  void (symbol name)
  bindings <- many pPattern <?> "pattern"
  void (symbolN "=")
  expr <- pExpr
  pure Def { defArgs = bindings, defExpr = expr }

-- Like pDef but will parse a definition with any name
pDef' :: Parser (RawName, Def Syn)
pDef' = do
  name     <- lexeme lowercaseName
  bindings <- many pPattern <?> "pattern"
  void (symbolN "=")
  expr <- pExpr
  pure (name, Def { defArgs = bindings, defExpr = expr })

-- Int
-- Maybe Int
-- a
-- a -> b
pType :: Parser Type
pType = try arr <|> try app <|> pType'
 where
  arr = do
    a <- lexemeN (try app <|> pType')
    void $ symbolN "->"
    TyFun a <$> pType
  app = (lowercaseName >>= varApp) <|> (uppercaseName >>= conApp)
  conApp name = do
    rest <- many (lexeme pType')
    pure $ TyCon name rest
  varApp name = do
    rest <- many (lexeme pType')
    pure $ if null rest then TyVar name else TyCon name rest
  pType' = con <|> var <|> hole <|> list <|> record <|> try tuple <|> parens pType
  var    = TyVar <$> lowercaseName
  con    = (`TyCon` []) <$> uppercaseName
  hole   = TyHole <$> (string "?" >> pHoleName)
  list   = TyList <$> brackets pType
  tuple  = TyTuple <$> parens (lexemeN pType `sepBy2` comma)
  record = TyRecord <$> braces (recordField `sepBy1` comma)
  recordField = do
    fName <- lowercaseName
    void (symbol ":")
    ty <- pType
    pure (fName, ty)

-- Parses the type args to a constructor
-- The rules are slightly different from types in annotations, because type
-- application must be inside parentheses
-- i.e. MyCon f a   -> name = MyCon, args = [f, a]
-- vs.  func : f a  -> name = func, type = f :@: a
pConType :: Parser Type
pConType = ty
 where
  ty  = hole <|> var <|> con <|> list <|> record <|> parens (try arr <|> try tuple <|> app)
  app = (lowercaseName >>= varApp) <|> (uppercaseName >>= conApp)
  conApp name = do
    rest <- many ty
    pure $ TyCon name rest
  varApp name = do
    rest <- many ty
    pure $ if null rest then TyVar name else TyCon name rest
  arr = do
    a <- lexemeN ty
    void $ symbolN "->"
    TyFun a <$> ty
  con   = TyCon <$> uppercaseName <*> pure []
  var   = TyVar <$> lowercaseName
  list  = TyList <$> brackets ty
  tuple = TyTuple <$> ty `sepBy2` comma
  hole  = TyHole <$> (string "?" >> pHoleName)
  record = TyRecord <$> braces (recordField `sepBy1` comma)
  recordField = do
    fName <- lowercaseName
    void (symbol ":")
    ty <- pType
    pure (fName, ty)

pPattern :: Parser Pattern
pPattern = pPattern' <|> cons
 where
  tyCon          = uppercaseName
  cons           = try nullaryCon <|> try infixBinaryCon <|> con
  nullaryCon     = ConsPat <$> tyCon <*> pure []
  infixBinaryCon = parens $ do
    left  <- pPattern
    tycon <- binTyCon
    right <- pPattern
    pure $ ConsPat tycon [left, right]
  -- For now, the only infix constructor is (::)
  binTyCon = Name <$> symbol "::"
  con      = parens $ do
    c    <- tyCon
    args <- many pPattern
    pure $ ConsPat c args

pPattern' :: Parser Pattern
pPattern' = try pIntPat <|> pWildPat <|> pListPat <|> try pTuplePat <|> pVarPat

-- Case patterns differ from function patterns in that a constructor pattern
-- doesn't have to be in parentheses (because we are only scrutinising a single
-- expression).
-- e.g. case foo of
--        Just x -> ...
-- is valid whereas
-- foo Just x = ...
-- is not the same as
-- foo (Just x) = ...
pCasePattern :: Parser Pattern
pCasePattern =
  parens (try infixBinaryCon <|> tuplePattern <|> pPattern)
    <|> pIntPat
    <|> pWildPat
    <|> pListPat
    <|> con
    <|> pVarPat
 where
  tuplePattern   = TuplePat <$> pPattern `sepBy` comma
  con            = ConsPat <$> uppercaseName <*> many pPattern
  infixBinaryCon = do
    left  <- pPattern
    tycon <- Name <$> symbol "::"
    right <- pPattern
    pure $ ConsPat tycon [left, right]

pIntPat :: Parser Pattern
pIntPat = IntPat <$> pInt

pWildPat :: Parser Pattern
pWildPat = symbol "_" >> pure WildPat

pListPat :: Parser Pattern
pListPat = ListPat <$> brackets (pPattern `sepBy` comma)

pTuplePat :: Parser Pattern
pTuplePat = TuplePat <$> parens (pPattern `sepBy` comma)

pVarPat :: Parser Pattern
pVarPat = VarPat <$> lowercaseName

pInt :: Parser Int
pInt = do
  sign   <- optional (string "-")
  digits <- lexeme (some digitChar)
  pure . read $ fromMaybe "" sign <> digits

pExpr :: Parser Syn
pExpr = try pBinApp <|> try pApp <|> pExpr'

pExpr' :: Parser Syn
pExpr' =
  try pTuple
    <|> parens pExpr
    <|> pRecord
    <|> pHole
    <|> try pStringLit
    <|> try (IntLit <$> pInt)
    <|> try pRecordProject
    <|> pVar
    <|> pAbs
    <|> pLet
    <|> pList
    <|> pCons
    <|> pCase

-- Application of a binary operator
-- e.g. x + y
-- TODO: fixity?
-- Maybe handle this after parsing
pBinApp :: Parser Syn
pBinApp = do
  left <- pExpr'
  op   <- pOp
  void $ some (char ' ')
  right <- pExpr'
  pure $ App (App op left) right
 where
  pOp :: Parser Syn
  pOp =
    (Con . Name <$> string "::") <|> (Var . Name <$> (twoCharOp <|> oneCharOp))
  twoCharOp =
    string "&&" <|> string "||" <|> string ">=" <|> string "<=" <|> string "<>"
  oneCharOp = (: []) <$> oneOf ['+', '-', '*', '/', '>', '<']

pApp :: Parser Syn
pApp = do
  first <- pExpr'
  rest  <- some pExpr'
  pure $ foldl1 App (first : rest)

-- foo.bar
-- For easy of implementation we currently only support using projection on
-- variables. In the future we may want to support arbitrary expressions, e.g.
--   (let a = 1 in Foo { x = a }).x
pRecordProject :: Parser Syn
pRecordProject = do
  record <- pVar
  void (string ".")
  field <- lowercaseName
  pure $ Project record field

pHole :: Parser Syn
pHole = do
  void (string "?")
  Hole <$> pHoleName

-- "hello"
-- "hello \"friend\""
-- "hello backslash: \\"
-- "hello #{name}"
-- "hello #{name + "!"}"
pStringLit :: Parser Syn
pStringLit = do
  void (string "\"")
  (prefix, interps) <- pInner
  void (symbol "\"")
  pure $ StringLit prefix interps
 where
  pRawString :: Parser String
  pRawString = do
    beforeEscape <- takeWhileP Nothing (\c -> c /= '"' && c /= '#' && c /= '\\')
    afterEscape  <- optional $ do
      esc  <- pEscape
      rest <- pRawString
      pure (esc ++ rest)
    pure $ beforeEscape ++ fromMaybe "" afterEscape
  pEscape :: Parser String
  pEscape =
    string "\\" >> (string "\"" <|> string "\\" <|> (string "n" >> pure "\n"))
  pInterp :: Parser Syn
  pInterp = do
    void (string "#{")
    e <- pExpr
    void (string "}")
    return e
  pInner :: Parser (String, [(Syn, String)])
  pInner = do
    prefix <- pRawString
    -- after the #, we either have a string interpolation or just more raw string
    next   <- Left <$> pInterp <|> Right <$> pRawString
    case next of
      Left e -> do
        (str, interps) <- pInner
        pure (prefix, (e, str) : interps)
      Right s
        | null s -> pure (prefix, [])
        | otherwise -> do
          (str, interps) <- pInner
          pure (prefix <> s <> str, interps)

pVar :: Parser Syn
pVar = Var <$> lowercaseName

pAbs :: Parser Syn
pAbs = do
  void (string "\\")
  args <- many lowercaseName
  void (symbol "->")
  Abs <$> pure args <*> pExpr

-- let foo = 1
--     bar = 2
--  in x
pLet :: Parser Syn
pLet = do
  void (symbolN "let")
  binds <- many (lexemeN pBind)
  void (symbolN "in")
  Let binds <$> pExpr
 where
  pBind :: Parser (RawName, Syn)
  pBind = do
    var <- lowercaseName
    void (symbol "=")
    val <- pExpr
    lexemeN (void newline)
    pure (var, val)

pTuple :: Parser Syn
pTuple = TupleLit <$> parens (pExpr `sepBy2` comma)

pList :: Parser Syn
pList = ListLit
  <$> between (symbolN "[") (symbol "]") (lexemeN pExpr `sepBy` lexemeN comma)

pCons :: Parser Syn
pCons = Con <$> uppercaseName

pCase :: Parser Syn
pCase = do
  void (symbol "case")
  scrutinee <- pExpr
  void (symbol "of")
  void newline
  indentation <- some (char ' ')
  first       <- pAlt
  rest        <- many $ try (newline >> string indentation >> pAlt)
  pure $ Case scrutinee (first : rest)
 where
  pAlt :: Parser (Pattern, Syn)
  pAlt = do
    pat <- pCasePattern
    void (symbol "->")
    expr <- pExpr
    pure (pat, expr)

pRecord :: Parser Syn
pRecord = Record <$> braces (pField `sepBy1` comma)
  where pField = do name <- lowercaseName
                    void (symbol "=")
                    expr <- pExpr
                    pure (name, expr)

pComment :: Parser String
pComment = do
  void (symbol "-- " <|> symbol "--")
  s <- takeWhileP (Just "comment") (/= '\n')
  spaceConsumerN
  pure s

pName :: Parser RawName
pName = uppercaseName <|> lowercaseName

pModuleName :: Parser ModuleName
pModuleName = ModuleName <$> lexeme (uppercaseString' `sepBy` string ".")
 where
  -- like uppercaseString but doesn't consume trailing space
  uppercaseString' :: Parser String
  uppercaseString' = (:) <$> upperChar <*> many alphaNumChar

pHoleName :: Parser RawName
pHoleName = lexeme $ Name <$> do
  s <- some alphaNumChar
  guard (s `notElem` keywords)
  pure s

uppercaseName :: Parser RawName
uppercaseName = lexeme $ Name <$> do
  t <- (:) <$> upperChar <*> many alphaNumChar
  guard (t `notElem` keywords)
  pure t

lowercaseName :: Parser RawName
lowercaseName = Name <$> lowercaseString

lowercaseString :: Parser String
lowercaseString = lexeme . try $ do
  t <- (:) <$> (lowerChar <|> char '$') <*> many alphaNumChar
  guard (t `notElem` keywords)
  pure t

keywords :: [String]
keywords =
  [ "type"
  , "alias"
  , "from"
  , "qualified"
  , "as"
  , "let"
  , "in"
  , "case"
  , "of"
  , "where"
  , "module"
  , "import"
  ]

-- Consumes spaces and tabs
spaceConsumer :: Parser ()
spaceConsumer = L.space (void $ some (char ' ')) empty empty

-- Consumes spaces, tabs and newlines
spaceConsumerN :: Parser ()
spaceConsumerN = L.space (void (some spaceChar)) empty empty

-- Parses a specific string, skipping trailing spaces and tabs
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- Like symbol but also skips trailing newlines
symbolN :: String -> Parser String
symbolN = L.symbol spaceConsumerN

-- Runs the given parser, skipping trailing spaces and tabs
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- Like lexeme but also skips trailing newlines
lexemeN :: Parser a -> Parser a
lexemeN = L.lexeme spaceConsumerN

parens :: Parser p -> Parser p
parens = between (symbol "(") (symbol ")")

braces :: Parser p -> Parser p
braces = between (symbol "{") (symbol "}")

brackets :: Parser p -> Parser p
brackets = between (symbol "[") (symbol "]")

comma :: Parser String
comma = symbol ","

-- Like sepBy1 but parses at least _two_ occurrences of p
-- Useful for when you need to be sure you have a tuple type rather than just a
-- variable in parentheses
sepBy2 :: Parser a -> Parser sep -> Parser [a]
sepBy2 p sep = do
  first <- p
  void sep
  rest <- p `sepBy1` sep
  pure (first : rest)
{-# INLINE sepBy2 #-}
