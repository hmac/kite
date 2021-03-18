{-# LANGUAGE FlexibleContexts #-}
module Syn.Parse where

import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import           Data.Functor                   ( void, ($>) )
import           Control.Monad                  ( guard )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     ( indentGuard
                                                , indentLevel
                                                , nonIndented
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L


import qualified Syn
import           Syn                     hiding ( Pattern )
import           AST

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

  Indentation is currently fairly robust, but stricter than Haskell.

  Specifically, we require that lexemes following a 'case', 'mcase' or 'let' are aligned on a column
  which is greater than the column of the last character of these keywords.

  For example, this is valid:

      myFunction = x -> case x of
                         Just y -> True
                         Nothing -> False

  But these are not valid:

      myFunction = x -> case x of
                     Just y -> True
                     Nothing -> False

      myFunction = x -> case x of
        Just y -> True
        Nothing -> False

  In Haskell this is permitted[0].

  I would like to relax this restriction, but it significantly complicates the parser so I'm leaving
  it for now.


  0: https://www.haskell.org/onlinereport/lexemes.html#sect2.7
-}

type Parser = Parsec Error String

newtype Error = VarKeyword String
  deriving (Eq, Ord, Show)

instance ShowErrorComponent Error where
  showErrorComponent (VarKeyword v) =
    show v <> " is a reserved keyword and cannot be used as a variable name."

parseKiteFile :: FilePath -> String -> Either String Module
parseKiteFile path input = case parse (pModule <* eof) path input of
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
  nonIndented spaceConsumerN $
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
  p0 <- indentLevel
  name     <- lowercaseName <?> "function name"
  sig      <- symbol ":" >> pType
  indentGEQ_ p0
  _        <- lexeme lowercaseName >>= \n -> guard (name == n)
  void $ symbolN "="
  indentGT_ p0
  expr <- pExpr
  whereClause <- optional $ do
    indentGT_ p0
    pos <- indentLevel
    void $ symbolN "where"
    many (indentGT_ pos >> pFun)

  pure Fun { funComments = comments
           , funName     = name
           , funType     = Just sig
           , funExpr     = expr
           , funWheres   = fromMaybe [] whereClause
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
    void (symbolN ":")
    ty <- pType' Neutral
    spaceConsumerN
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

pPattern :: Parser Syn.Pattern
pPattern = try pPattern' <|> pConPat True

pPattern' :: Parser Syn.Pattern
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
pCasePattern :: Parser Syn.Pattern
pCasePattern =
  try (pConPat False)
    <|> pUnitPat
    <|> parens tuplePattern
    <|> pIntPat
    <|> pCharPat
    <|> pWildPat
    <|> pListPat
    <|> pVarPat
 where
  tuplePattern = TuplePat <$> pPattern `sepBy2` comma

pIntPat :: Parser Syn.Pattern
pIntPat = IntPat <$> pInt

pCharPat :: Parser Syn.Pattern
pCharPat = CharPat <$> pChar

pWildPat :: Parser Syn.Pattern
pWildPat = symbol "_" >> pure WildPat

pListPat :: Parser Syn.Pattern
pListPat = ListPat <$> brackets (pPattern `sepBy` comma)

pUnitPat :: Parser Syn.Pattern
pUnitPat = symbol "()" >> pure UnitPat

pTuplePat :: Parser Syn.Pattern
pTuplePat = TuplePat <$> parens (pPattern `sepBy2` comma)

pVarPat :: Parser Syn.Pattern
pVarPat = VarPat <$> lowercaseName

-- Parse a constructor pattern
-- The argument specifies whether we require parentheses around constructor patterns with arguments,
-- like (Just x).
-- We don't need these in case expressions, but we do in multi-case expressions.
pConPat :: Bool -> Parser Syn.Pattern
pConPat needParens = if needParens
                      then try nullaryCon <|> parens (try infixBinaryCon <|> con)
                      else try infixBinaryCon <|> con <|> parens (pConPat False)
 where
  tyCon      = uppercaseName
  nullaryCon = do
    name <- tyCon
    pure $ case name of
      "True"  -> BoolPat True
      "False" -> BoolPat False
      n       -> ConsPat n Nothing []
  infixBinaryCon = do
    left  <- pPattern
    tycon <- binTyCon
    right <- pPattern
    pure $ ConsPat tycon Nothing [left, right]
  -- For now, the only infix constructor is (::)
  binTyCon = Name <$> symbol "::"
  con      = do
    tyCon >>= \case
      "True"  -> pure $ BoolPat True
      "False" -> pure $ BoolPat False
      c -> do
            args <- many pPattern
            pure $ ConsPat c Nothing args

pInt :: Parser Int
pInt = do
  sign   <- optional (string "-")
  digits <- lexeme (some digitChar)
  spaceConsumerN
  pure . read $ fromMaybe "" sign <> digits

pExpr :: Parser Syn
pExpr = try pMultiCase <|> try pBinApp <|> try pApp <|> pExpr'

pExpr' :: Parser Syn
pExpr' =
  pUnitLit
    <|> try pTuple
    <|> parensN pExpr
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
  void $ symbolN "()"
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
  pos <- indentLevel
  first <- pExpr'
  rest <- some (indentGT pos >> pExpr')
  pure $ foldl1 App (first : rest)

-- foo.bar
-- For ease of implementation we currently only support using projection on
-- variables. In the future we may want to support arbitrary expressions, e.g.
--   (let a = 1 in Foo { x = a }).x
pRecordProject :: Parser Syn
pRecordProject = do
  record <- pVar
  void (string ".")
  field <- lowercaseString
  spaceConsumerN
  pure $ Project record field

pHole :: Parser Syn
pHole = do
  void (string "?")
  name <- pHoleName
  spaceConsumerN
  pure $ Hole name

pCharLit :: Parser Syn
pCharLit = do
  c <- pChar
  spaceConsumerN
  pure $ CharLit c

pChar :: Parser Char
pChar = between (string "'") (symbol "'") $ escapedChar <|> fmap head (takeP (Just "char") 1)
  -- Escaped special characters like \n
  -- Currently we only support \n
  -- TODO: what's the full list of escape sequences we should support here?
    where escapedChar = char '\\' >> char 'n' $> '\n'

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
  void (symbolN "\"")
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
      in  if null interps
            then StringLit (prefix <> prefix')
            else StringInterp (prefix <> prefix') interps
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
pVar = do
  v <- Var <$> lowercaseName
  spaceConsumerN
  pure v

pFCall :: Parser Syn
pFCall = do
  p0 <- indentLevel
  void (symbolN "$fcall")
  indentGT_ p0
  name <- lexemeN lowercaseName
  args <- many (indentGT p0 >> pExpr')
  pure $ FCall (unName name) args

pAbs :: Parser Syn
pAbs = do
  void (string "\\")
  args <- many lowercaseName
  void (symbol "->")
  Abs args <$> pExpr

-- let foo = 1 in foo
--
-- let foo = 1
--     bar = 2
--  in foo
--
-- let foo = 1
--      bar = 2
--  in foo
--
-- let foo : Int
--     foo = 1
--     bar : Bool -> Int
--     bar = True -> 1
--           False -> 0
--  in foo
pLet :: Parser Syn
pLet = do
  p0 <- indentLevel
  void (symbol "let")
  p1 <- indentGT p0
  first <- pAnnotatedBind <|> pBind
  rest <- many $ do
    indentEQ_ p1
    pAnnotatedBind <|> pBind
  indentGT_ p0
  void (symbolN "in")
  Let (first:rest) <$> pExpr
 where
  pBind :: Parser (RawName, Syn, Maybe Type)
  pBind = do
    var <- lowercaseName
    void (symbolN "=")
    val <- lexemeN pExpr
    pure (var, val, Nothing)
  pAnnotatedBind :: Parser (RawName, Syn, Maybe Type)
  pAnnotatedBind = do
    (var, ty) <- try pAnnotation
    var' <- lowercaseName
    guard (var' == var)
    void (symbolN "=")
    val <- lexemeN pExpr
    pure (var, val, Just ty)
  pAnnotation :: Parser (RawName, Type)
  pAnnotation = do
    var <- lowercaseName
    void (symbolN ":")
    ty <- lexemeN pType
    pure (var, ty)

pTuple :: Parser Syn
pTuple = do
  pos <- indentLevel
  _   <- symbol "("
  indentGT_ pos
  expr1 <- pExpr
  exprs <- some $ do
    indentGEQ_ pos
    _ <- comma
    indentGT_ pos
    pExpr
  spaceConsumerN
  _ <- symbolN ")"
  pure $ TupleLit (expr1 : exprs)

makePositive :: Int -> Int
makePositive n | n < 1     = 1
               | otherwise = n

pList :: Parser Syn
pList = ListLit
  <$> bracketsN (lexemeN pExpr `sepBy` lexemeN comma)

pCons :: Parser Syn
pCons = do
  name <- uppercaseName
  spaceConsumerN
  pure $ case name of
    "True"  -> BoolLit True
    "False" -> BoolLit False
    n       -> Con n

pCase :: Parser Syn
pCase = do
  pos <- indentLevel
  void (symbol "case")
  scrutinee <- pExpr
  void (symbolN "of")
  alts <- some $ do
    try $ indentGT_ pos
    pAlt
  pure $ Case scrutinee alts
 where
  pAlt :: Parser (Syn.Pattern, Syn)
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
  pos <- indentLevel
  alts <- some $ do
    indentGEQ_ pos
    pAlt
  spaceConsumerN
  pure $ MCase alts
 where
  pAlt :: Parser ([Syn.Pattern], Syn)
  pAlt = do
    pos <- indentLevel
    pats <- some $ do
      indentGEQ_ pos
      pPattern
    indentGT_ pos
    void (symbolN "->")
    indentGT_ pos
    expr <- pExpr
    pure (pats, expr)


pRecord :: Parser Syn
pRecord = Record <$> bracesN (pField `sepBy1` symbolN ",")
 where
  pField = do
    name <- lowercaseString
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

-- | Consume whitespace and newlines, then check that the indentation level is equal to the given
-- value. Returns the indentation level.
indentEQ :: Pos -> Parser Pos
indentEQ = indentGuard spaceConsumerN EQ

-- | Consume whitespace and newlines, then check that the indentation level is greater than the
-- given value. Returns the indentation level.
indentGT :: Pos -> Parser Pos
indentGT = indentGuard spaceConsumerN GT

-- | Consume whitespace and newlines, then check that the indentation level is greater than or equal
-- to the given value. Returns the indentation level.
indentGEQ :: Pos -> Parser Pos
indentGEQ pos = try (indentGT pos) <|> indentEQ pos

-- | Like 'indentEQ' but returns @()@.
indentEQ_ :: Pos -> Parser ()
indentEQ_ = void . indentEQ

-- | Like 'indentGT' but returns @()@.
indentGT_ :: Pos -> Parser ()
indentGT_ = void . indentGT

-- | Like 'indentGEQ' but returns @()@.
indentGEQ_ :: Pos -> Parser ()
indentGEQ_ = void . indentGEQ

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

-- Skip zero or more newlines
skipNewlines :: Parser ()
skipNewlines = skipMany newline

parens :: Parser p -> Parser p
parens = between (symbol "(") (symbol ")")

parensN :: Parser p -> Parser p
parensN = between (symbolN "(") (symbolN ")")

braces :: Parser p -> Parser p
braces = between (symbol "{") (symbol "}")

bracesN :: Parser p -> Parser p
bracesN = between (symbolN "{") (symbolN "}")

brackets :: Parser p -> Parser p
brackets = between (symbol "[") (symbol "]")

bracketsN :: Parser p -> Parser p
bracketsN = between (symbolN "[") (symbolN "]")

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
