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
  void $ symbol "module"
  name    <- lexemeN uppercaseName
  exports <- lexemeN . parens $ pName `sepBy` comma
  imports <- many (lexemeN pImport)
  decls   <- many (lexemeN pDecl)
  pure $ Module { moduleName    = name
                , moduleImports = imports
                , moduleExports = exports
                , moduleDecls   = decls
                }

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

-- TODO: we can make this more flexible by allowing annotations separate from
-- definitions
pDecl :: Parser Decl
pDecl = do
  name       <- lowercaseName <?> "declaration type name"
  annotation <- symbol ":" >> lexemeN pType
  defs       <- many (lexemeN (pDef name))
  pure Decl { declName = Name name, declType = annotation, declDefs = defs }

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
  arr    = TyArr <$> pType' <*> (symbol "->" >> pType')
  pType' = parens pType <|> cons <|> var <|> list <|> tuple
  cons   = TyCon <$> (Name <$> uppercaseName) <*> many pType'
  var    = TyVar . Name <$> lowercaseName
  list   = TyList <$> brackets pType'
  tuple  = TyTuple <$> parens (pType' `sepBy` comma)

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
lowercaseName = lexeme $ do
  t <- (:) <$> lowerChar <*> many alphaNumChar
  guard (t `notElem` keywords)
  pure t

keywords :: [String]
keywords = ["case", "module", "import"]

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

-- Runs the given parser, skipping trailing spaces and tabs
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- Like lexeme but also skips newlines
lexemeN :: Parser a -> Parser a
lexemeN = L.lexeme spaceConsumerN

parens :: Parser p -> Parser p
parens = between (symbol "(") (symbol ")")

brackets :: Parser p -> Parser p
brackets = between (symbol "[") (symbol "]")

comma :: Parser String
comma = symbol ","
