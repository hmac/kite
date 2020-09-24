module Syn.Parse where

import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import           Data.Functor                   ( void )
import           Control.Monad                  ( guard )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     ( indentGuard
                                                , indentLevel
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L

import           Syn

-- TODO: markdown in comments & doctests
-- TODO: heredocs
-- TODO: do notation
-- TODO: where clause
-- TODO: record patterns
-- TODO: infix constructors (like List ::)
-- TODO: empty data types (e.g. Void)
-- TODO: include package in imports: from std import Data.Either


{-
  Indentation
  -----------

  Indentation is currently not handled well in the parser. I would like to use
  the `indentation` package but it hasn't been ported to megaparsec (and porting
  it proved tricky).

  Idris also uses Megaparsec for its parser, and they have developed their own
  indentation handling which I think we can steal from.

  The core idea seems to be to keep track of a stack of indentation positions in
  the parser state, pushing them on as we enter new syntactic structures and
  pushing off afterwards. We then use these positions to parse the correct
  amount of indentation after each new line.

  Links:
    https://github.com/idris-lang/Idris-dev/blob/master/src/Idris/Parser/Helpers.hs
    https://github.com/idris-lang/Idris-dev/blob/master/src/Idris/Parser/Stack.hs
    https://github.com/idris-lang/Idris-dev/blob/master/src/Idris/Parser/Expr.hs

-}

type Parser = Parsec Error String

newtype Error = VarKeyword String
  deriving (Eq, Ord, Show)

instance ShowErrorComponent Error where
  showErrorComponent (VarKeyword v) =
    show v <> " is a reserved keyword and cannot be used as a variable name."

parseLamFile :: String -> Either String Module
parseLamFile input = case parse (pModule <* eof) "" input of
  Left  e -> Left (errorBundlePretty e)
  Right e -> Right e

pModule :: Parser Module
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

-- We want comments above functions to be associated with them, but doing this
-- in the parser leads to some backtracking that worsens error messages and
-- reduces performance, so we keep it simple here. In a later stage of the
-- compiler we merge adjacent comment and function declarations.
pDecl :: Parser (Decl Syn)
pDecl =
  Comment
    <$> pComment
    <|> AliasDecl
    <$> pAlias
    <|> DataDecl
    <$> pData
    <|> FunDecl
    <$> pFun

pAlias :: Parser Alias
pAlias = do
  void (symbol' "type alias")
  alias  <- uppercaseName
  tyvars <- many lowercaseName
  void (symbolN "=")
  ty <- pType
  pure Alias { aliasName = alias, aliasTyVars = tyvars, aliasType = ty }

pData :: Parser Data
pData = do
  void (symbol' "type")
  name   <- uppercaseName
  tyvars <- many lowercaseName
  void (symbolN "=")
  constructors <- lexemeN pCon `sepBy` symbolN "|"
  pure Data { dataName = name, dataTyVars = tyvars, dataCons = constructors }
 where
  pCon :: Parser DataCon
  pCon = DataCon <$> uppercaseName <*> many pConType

-- Parse a function definition
-- Functions consist of a type signature, a newline, and an implementation
-- Unlike Haskell, functions only have a single equation.
-- Pattern matching happens on the RHS
pFun :: Parser (Fun Syn)
pFun = do
  comments <- many pComment
  name     <- lowercaseName <?> "declaration type name"
  sig      <- symbol ":" >> pType
  _        <- indentGuard spaceConsumerN EQ pos1
  _        <- lexeme lowercaseName >>= \n -> guard (name == n)
  _        <- void $ string "="
  _        <- indentGuard spaceConsumerN GT (mkPos 2)
  expr     <- pExpr
  pure Fun { funComments = comments
           , funName     = name
           , funType     = Just sig
           , funExpr     = expr
           }

-- The context for parsing a type
-- Paren means that compound types have to be in parens
-- Neutral means anything goes
data TypeCtx = Neutral | Paren | AppL | AppR

-- Int
-- Maybe Int
-- a
-- a -> b
pType :: Parser Type
pType = pType' Neutral

-- Note: currently broken
pType' :: TypeCtx -> Parser Type
pType' ctx = case ctx of
  Neutral ->
    try arr <|> try app <|> for_all <|> atomic <|> parens (pType' Neutral)
  Paren -> atomic <|> parens (pType' Neutral)
  AppL  -> try app <|> atomic <|> parens (pType' Neutral)
  AppR  -> atomic <|> parens (pType' Neutral)
 where
  atomic = unit <|> con <|> var <|> hole <|> list <|> record <|> try tuple
  arr    = do
    a <- lexemeN (try app <|> pType' Paren)
    void $ symbolN "->"
    TyFun a <$> pType' Neutral
  app = do
    l  <- pType' Paren
    rs <- some (pType' AppR)
    pure $ foldl TyApp l rs
  var  = TyVar <$> lowercaseName
  unit = symbol "()" >> pure TyUnit
  con  = do
    name <- uppercaseName
    pure $ case name of
      "String" -> TyString
      "Int"    -> TyInt
      "Bool"   -> TyBool
      "Char"   -> TyChar
      n        -> TyCon n
  hole = TyHole <$> (string "?" >> pHoleName)
  -- TODO: maybe use List instead of []?
  -- Simplifies the syntax of types
  list =
    (TyList <$ symbol "[]") <|> (TyApp TyList <$> brackets (pType' Neutral))
  tuple       = TyTuple <$> parens (lexemeN (pType' Neutral) `sepBy2` comma)
  record      = TyRecord <$> braces (recordField `sepBy1` comma)
  recordField = do
    fName <- lowercaseName
    void (symbol ":")
    ty <- pType' Neutral
    pure (fName, ty)
  for_all = do
    void (symbol "forall")
    vars <- some lowercaseName
    void (symbol ".")
    t <- pType' Neutral
    pure $ foldr TyForall t vars

-- When parsing the type args to a constructor, type application must be inside
-- parentheses
-- i.e. MyCon f a   -> name = MyCon, args = [f, a]
-- vs.  func : f a  -> name = func, type = f :@: a
pConType :: Parser Type
pConType = pType' Paren

pPattern :: Parser Pattern
pPattern = pPattern' <|> cons
 where
  tyCon      = uppercaseName
  cons       = try nullaryCon <|> try infixBinaryCon <|> con
  nullaryCon = do
    name <- tyCon
    pure $ case name of
      "True"  -> BoolPat True
      "False" -> BoolPat False
      n       -> ConsPat n []
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
pPattern' =
  try pIntPat
    <|> pWildPat
    <|> pCharPat
    <|> pListPat
    <|> pUnitPat
    <|> try pTuplePat
    <|> pVarPat

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
  pUnitPat
    <|> parens (try infixBinaryCon <|> tuplePattern <|> pPattern)
    <|> pIntPat
    <|> pCharPat
    <|> pWildPat
    <|> pListPat
    <|> con
    <|> pVarPat
 where
  tuplePattern = TuplePat <$> pPattern `sepBy2` comma
  con          = do
    name <- uppercaseName
    case name of
      "True"  -> pure $ BoolPat True
      "False" -> pure $ BoolPat False
      n       -> ConsPat n <$> many pPattern
  infixBinaryCon = do
    left  <- pPattern
    tycon <- Name <$> symbol "::"
    right <- pPattern
    pure $ ConsPat tycon [left, right]

pIntPat :: Parser Pattern
pIntPat = IntPat <$> pInt

pCharPat :: Parser Pattern
pCharPat = CharPat <$> pChar

pWildPat :: Parser Pattern
pWildPat = symbol "_" >> pure WildPat

pListPat :: Parser Pattern
pListPat = ListPat <$> brackets (pPattern `sepBy` comma)

pUnitPat :: Parser Pattern
pUnitPat = symbol "()" >> pure UnitPat

pTuplePat :: Parser Pattern
pTuplePat = TuplePat <$> parens (pPattern `sepBy2` comma)

pVarPat :: Parser Pattern
pVarPat = VarPat <$> lowercaseName

pInt :: Parser Int
pInt = do
  sign   <- optional (string "-")
  digits <- lexeme (some digitChar)
  pure . read $ fromMaybe "" sign <> digits

pExpr :: Parser Syn
pExpr = try pMultiCase <|> try pBinApp <|> try pApp <|> pExpr'

pExpr' :: Parser Syn
pExpr' =
  pUnitLit
    <|> try pTuple
    <|> parens pExpr
    <|> pRecord
    <|> pHole
    <|> pCharLit
    <|> try pStringLit
    <|> try (IntLit <$> pInt)
    <|> try pRecordProject
    <|> pVar
    <|> pFCall
    <|> pAbs
    <|> pLet
    <|> pList
    <|> pCons
    <|> pCase

pUnitLit :: Parser Syn
pUnitLit = do
  void $ symbol "()"
  pure UnitLit

-- Application of a binary operator
-- e.g. x + y
-- TODO: fixity?
-- Maybe handle this after parsing
pBinApp :: Parser Syn
pBinApp = do
  left <- pExpr'
  op   <- pOp
  void $ some (char ' ')
  App (App op left) <$> pExpr'
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
-- For ease of implementation we currently only support using projection on
-- variables. In the future we may want to support arbitrary expressions, e.g.
--   (let a = 1 in Foo { x = a }).x
pRecordProject :: Parser Syn
pRecordProject = do
  record <- pVar
  void (string ".")
  Project record <$> lowercaseName

pHole :: Parser Syn
pHole = do
  void (string "?")
  Hole <$> pHoleName

pCharLit :: Parser Syn
pCharLit = CharLit <$> pChar

pChar :: Parser Char
pChar = head <$> between (string "'") (symbol "'") (takeP (Just "char") 1)

-- String literals are quite complex. These are some of the variations we need
-- to handle:

-- "hello"
-- "hello \"friend\""
-- "hello backslash: \\"
-- "hello newline: \n"
-- "hello #{name}"
-- "hello #{name + "!"}"
-- "hello hash: #"
-- "hello hash bracket: #\{"

-- Represents a chunk of parsed literal string
data StrParse = Interp Syn | StrEnd | Str String
  deriving (Eq, Show)

pStringLit :: Parser Syn
pStringLit = do
  void (char '"')
  parts <- pInner
  void (symbol "\"")
  let
    first :: [StrParse] -> (String, [StrParse])
    first (Str s    : rest) = let (s', rest') = first rest in (s <> s', rest')
    first (StrEnd   : _   ) = ("", [])
    first (Interp e : rest) = ("", Interp e : rest)
    first []                = ("", [])

    comps :: [StrParse] -> (String, [(Syn, String)])
    comps (Interp e : rest) =
      let (s, rest') = comps rest in ("", (e, s) : rest')
    comps (Str s  : rest) = let (s', cs) = comps rest in (s <> s', cs)
    comps (StrEnd : _   ) = ("", [])
    comps []              = ("", [])

  pure
    $ let (prefix , rest   ) = first parts
          (prefix', interps) = comps rest
      in  StringLit (prefix <> prefix') interps
 where
  lexChar :: Parser String
  lexChar = do
    c <- anySingleBut '"' <?> "any character except a double quote"
    case c of
      '\\'  -> pEsc
      '#'   -> (single '{' >> pure "#{") <|> pure "#"
      other -> pure [other]
  pEsc :: Parser String
  pEsc =
    string "\"" <|> string "\\" <|> string "{" <|> (string "n" >> pure "\n")
  pRawString :: Parser StrParse
  pRawString = do
    s <- optional lexChar
    case s of
      -- we've reached the end of the string
      Nothing   -> pure StrEnd
      -- we've reached an interpolation
      Just "#{" -> do
        e <- pExpr
        _ <- string "}"
        pure (Interp e)
      Just other -> pure (Str other)
  pInner :: Parser [StrParse]
  pInner = do
    res <- pRawString
    case res of
      StrEnd   -> pure [StrEnd]
      Interp e -> do
        rest <- pInner
        pure $ Interp e : rest
      Str s -> do
        rest <- pInner
        pure $ Str s : rest

pVar :: Parser Syn
pVar = Var <$> lowercaseName

pFCall :: Parser Syn
pFCall = do
  void (symbol "$fcall")
  name <- lowercaseName
  args <- many pExpr'
  pure $ FCall (unName name) args

pAbs :: Parser Syn
pAbs = do
  void (string "\\")
  args <- many lowercaseName
  void (symbol "->")
  Abs args <$> pExpr

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
pTuple = do
  _     <- symbol "("
  pos   <- mkPos . makePositive . subtract 2 . unPos <$> indentLevel
  expr1 <- pExpr
  exprs <- some $ do
    _ <- indentGuard spaceConsumerN GT pos
    _ <- comma
    _ <- indentGuard spaceConsumerN GT pos
    pExpr
  spaceConsumerN
  _ <- symbol ")"
  pure $ TupleLit (expr1 : exprs)

makePositive :: Int -> Int
makePositive n | n < 1     = 1
               | otherwise = n

pList :: Parser Syn
pList = ListLit
  <$> between (symbolN "[") (symbol "]") (lexemeN pExpr `sepBy` lexemeN comma)

pCons :: Parser Syn
pCons = do
  name <- uppercaseName
  pure $ case name of
    "True"  -> BoolLit True
    "False" -> BoolLit False
    n       -> Con n

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

-- A multi-case is a lambda-case expression which can scrutinise multiple things
-- at once. It looks like this:
--     True [] -> 1
--     _    _  -> 2
--
-- This is equivalent to the haskell expression:
--
--   \b l -> case b of
--             True -> case l of
--                       [] -> 1
--                       _ -> 2
--             _    -> 2
--
-- Note that constructor patterns must be parenthesised to distinguish them from
-- multiple independent patterns. This is why we use pPattern rather than
-- pCasePattern.
pMultiCase :: Parser Syn
pMultiCase = do
  pos   <- mkPos . makePositive . subtract 1 . unPos <$> indentLevel
  first <- pAlt
  rest  <- many $ try $ do
    void $ indentGuard spaceConsumerN GT pos
    pAlt
  pure $ MCase (first : rest)
 where
  pAlt :: Parser ([Pattern], Syn)
  pAlt = do
    pats <- some pPattern
    void (symbol "->")
    expr <- pExpr
    pure (pats, expr)


pRecord :: Parser Syn
pRecord = Record <$> braces (pField `sepBy1` comma)
 where
  pField = do
    name <- lowercaseName
    void (symbol "=")
    expr <- pExpr
    pure (name, expr)

pComment :: Parser String
pComment = do
  void (string "-- " <|> string "--")
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
  if s `elem` keywords then customFailure (VarKeyword s) else pure s

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
  , "forall"
  , "$fcall"
  ]

-- Consumes spaces
spaceConsumer :: Parser ()
spaceConsumer = L.space (skipSome (char ' ')) empty empty

-- Fails unless it can consume at least one space
spaceConsumer' :: Parser ()
spaceConsumer' = char ' ' >> spaceConsumer

-- Consumes spaces and newlines
spaceConsumerN :: Parser ()
spaceConsumerN = L.space (skipSome spaceChar) empty empty

-- Fails unless it consumes at least one space or newline
spaceConsumerN' :: Parser ()
spaceConsumerN' = spaceChar >> spaceConsumerN

-- Parses a specific string, skipping trailing spaces
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- Parses a specific string, requiring at least one trailing space
symbol' :: String -> Parser String
symbol' = L.symbol spaceConsumer'

-- Like symbol but also skips trailing newlines
symbolN :: String -> Parser String
symbolN = L.symbol spaceConsumerN

-- Like symbol' but also skips trailing newlines
symbolN' :: String -> Parser String
symbolN' = L.symbol spaceConsumerN

-- Runs the given parser, skipping trailing spaces
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- Like lexeme but also skips trailing newlines
lexemeN :: Parser a -> Parser a
lexemeN = L.lexeme spaceConsumerN

-- Skip at least two space characters
indent :: Parser ()
indent = char ' ' >> skipSome (char ' ')

-- Skip one or more space characters
someSpace :: Parser ()
someSpace = skipSome (char ' ')

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
