module Parse where

import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import           Data.Void                      ( Void )
import           Data.Functor                   ( void )
import           Control.Monad                  ( guard
                                                , when
                                                )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Syntax

type Parser = Parsec Void String

parseLamFile :: String -> Either String Module
parseLamFile input = case parse (pModule <* eof) "" input of
  Left  e -> Left (errorBundlePretty e)
  Right e -> Right e

pModule :: Parser Module
pModule = do
  metadata <- optional pMetadata
  void $ symbol "module"
  name    <- lexemeN uppercaseName
  exports <- optional . lexemeN . parens $ pName `sepBy` comma
  imports <- many (lexemeN pImport)
  decls   <- many (lexemeN pDecl)
  pure $ Module { moduleName     = name
                , moduleImports  = imports
                , moduleExports  = fromMaybe [] exports
                , moduleDecls    = decls
                , moduleMetadata = fromMaybe [] metadata
                }

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
    key <- lowercaseName
    void (symbol ":")
    val <- many alphaNumChar
    void newline
    pure (key, val)

-- import Bar
-- import qualified Baz as Boo (fun1, fun2)
pImport :: Parser Import
pImport = do
  void $ symbol "import"
  qualified <- isJust <$> optional (symbol "qualified")
  name      <- pModuleName
  alias     <- optional (symbol "as" >> Name <$> uppercaseName)
  items     <- optional $ parens ((Name <$> lowercaseName) `sepBy` comma)
  pure Import { importQualified = qualified
              , importName      = name
              , importAlias     = alias
              , importItems     = fromMaybe [] items
              }

pDecl :: Parser Decl
pDecl =
  TypeclassDecl <$> pTypeclass <|> DataDecl <$> pData <|> FunDecl <$> pFun

pData :: Parser Data
pData = do
  void (symbol "data")
  name   <- Name <$> uppercaseName
  tyvars <- many (Name <$> lowercaseName)
  void (symbolN "=")
  constructors <- lexemeN pCon `sepBy` symbolN "|"
  pure Data { dataName = name, dataTyVars = tyvars, dataCons = constructors }
 where
  pCon :: Parser DataCon
  pCon = DataCon <$> (Name <$> uppercaseName) <*> many pConType

-- TODO: we can make this more flexible by allowing annotations separate from
-- definitions
pFun :: Parser Fun
pFun = do
  name       <- lowercaseName <?> "declaration type name"
  annotation <- symbol ":" >> lexemeN pType
  defs       <- many (lexemeN (pDef name))
  pure Fun { funName = Name name, funType = annotation, funDefs = defs }

-- TODO: indentation
pTypeclass :: Parser Typeclass
pTypeclass = do
  void (symbol "class")
  name   <- Name <$> uppercaseName
  tyvars <- many (Name <$> lowercaseName)
  void (symbolN "where")
  defs <- many pTypeclassDef
  pure Typeclass { typeclassName   = name
                 , typeclassTyVars = tyvars
                 , typeclassDefs   = defs
                 }
 where
  pTypeclassDef :: Parser (Name, Ty)
  pTypeclassDef = do
    name       <- Name <$> lowercaseName
    annotation <- symbol ":" >> lexeme pType
    void newline
    pure (name, annotation)

-- TODO: currently we can only parse definitions on a single line. Add support
-- for indentation and the off-side rule.
pDef :: String -> Parser Def
pDef name = do
  void (symbol name)
  bindings <- many pPattern <?> "pattern"
  void (symbol "=")
  expr <- pExpr
  pure Def { defArgs = bindings, defExpr = expr }

-- Int
-- Maybe Int
-- a
-- a -> b
pType :: Parser Ty
pType = try arr <|> pType'
 where
  arr    = TyArr <$> pType' <*> (symbol "->" >> pType)
  pType' = parens pType <|> try app <|> var <|> list <|> tuple
  app = TyApp <$> (Name <$> (uppercaseName <|> lowercaseName)) <*> some pType'
  var    = TyVar . Name <$> (uppercaseName <|> lowercaseName)
  list   = TyList <$> brackets pType'
  tuple  = TyTuple <$> parens (pType' `sepBy` comma)

-- Parses the type args to a constructor
-- The rules are slightly different from types in annotations, because type
-- application must be inside parentheses
-- i.e. MyCon f a   -> name = MyCon, args = [f, a]
-- vs.  func : f a  -> name = func, type = TyApp f a
pConType :: Parser Ty
pConType = ty
 where
  ty    = var <|> list <|> parens (try arr <|> try tuple <|> app)
  arr   = TyArr <$> ty <*> (symbol "->" >> ty)
  app   = TyApp <$> (Name <$> (uppercaseName <|> lowercaseName)) <*> some ty
  var   = TyVar . Name <$> (uppercaseName <|> lowercaseName)
  list  = TyList <$> brackets ty
  tuple = TyTuple <$> ty `sepBy2` comma

pPattern :: Parser Pattern
pPattern = lit <|> wild <|> list <|> try tuple <|> cons <|> var
 where
  lit        = LitPat <$> pLiteral
  wild       = symbol "_" >> pure WildPat
  list       = ListPat <$> brackets (pPattern `sepBy` comma)
  tuple      = TuplePat <$> parens (pPattern `sepBy` comma)
  cons       = try nullaryCon <|> con
  nullaryCon = ConsPat <$> tyCon <*> pure []
  con        = parens $ do
    c    <- tyCon
    args <- many pPattern
    pure $ ConsPat c args
  var   = VarPat . Name <$> lowercaseName
  tyCon = Name <$> uppercaseName

pLiteral :: Parser Literal
pLiteral = try floatLit <|> intLit <|> stringLit
 where
  intLit   = LitInt . read <$> lexeme (some digitChar)
  floatLit = do
    numeral <- many digitChar
    void (string ".")
    decimal <- lexeme (some digitChar)
    pure . LitFloat . read $ numeral <> "." <> decimal
  -- TODO: escape quote chars in string literals
  stringLit = do
    void (string "\"")
    str <- takeWhileP Nothing (/= '"')
    void (symbol "\"")
    pure (LitString str)

pExpr :: Parser Syn
pExpr = try pApp <|> pExpr'

pExpr' :: Parser Syn
pExpr' =
  parens pExpr
    <|> Lit
    <$> pLiteral
    <|> pVar
    <|> pAbs
    <|> pLet
    <|> pTuple
    <|> pList

pApp :: Parser Syn
pApp = App <$> pExpr' <*> pExpr

pVar :: Parser Syn
pVar = Var . Name <$> lowercaseName

pAbs :: Parser Syn
pAbs = do
  void (string "\\")
  args <- map Name <$> many lowercaseName
  void (symbol "->")
  Abs <$> pure args <*> pExpr

pLet :: Parser Syn
pLet = fail "cannot parse let"

pTuple :: Parser Syn
pTuple = fail "cannot parse tuple"

pList :: Parser Syn
pList = fail "cannot parse list"

pName :: Parser Name
pName = Name <$> (uppercaseName <|> lowercaseName)

pModuleName :: Parser ModuleName
pModuleName = ModuleName <$> lexeme (uppercaseName' `sepBy` string ".")
 where
  -- like uppercaseName but doesn't consume trailing space
  uppercaseName' :: Parser String
  uppercaseName' = (:) <$> upperChar <*> many letterChar

uppercaseName :: Parser String
uppercaseName = lexeme $ do
  t <- (:) <$> upperChar <*> many alphaNumChar
  guard (t `notElem` keywords)
  pure t

lowercaseName :: Parser String
lowercaseName = lexeme . try $ do
  t <- (:) <$> lowerChar <*> many alphaNumChar
  guard (t `notElem` keywords)
  pure t

keywords :: [String]
keywords = ["case", "module", "import", "where"]

-- Consumes spaces and tabs
spaceConsumer :: Parser ()
spaceConsumer =
  L.space (void $ some (char ' ' <|> char '\t')) (L.skipLineComment "--") empty

-- Consumes spaces, tabs and newlines
spaceConsumerN :: Parser ()
spaceConsumerN = L.space (void (some spaceChar)) (L.skipLineComment "--") empty

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
