module Syn.Parse.Expr
  ( pExpr
  ) where

import           Control.Monad                  ( guard )
import           Data.Functor                   ( void )
import qualified Data.List.NonEmpty            as NE

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     ( indentLevel )

import           AST
import           Syn
import           Syn.Parse.Common
import           Syn.Parse.Pattern              ( pCasePattern
                                                , pPattern
                                                )
import           Syn.Parse.Type                 ( pType )

pExpr :: Parser Syn
pExpr = try pMultiCase <|> try pBinApp <|> pApp

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

-- | Parse one or more expressions, forming an application.
-- If we can only parse one expression then we just return that, with no application.
pApp :: Parser Syn
pApp = do
  pos   <- indentLevel
  first <- pExpr'
  rest  <- many (indentGT pos >> pExpr')
  pure $ foldl App first rest

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
      in  case NE.nonEmpty interps of
            Just is -> StringInterp (prefix <> prefix') is
            Nothing -> StringLit (prefix <> prefix')
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

-- TODO: patterns in lambda bindings
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
  void (string "let")
  p1    <- indentGT p0
  first <- pAnnotatedBind <|> pBind
  rest  <- many $ do
    indentEQ_ p1
    pAnnotatedBind <|> pBind
  indentGT_ p0
  void (string "in")
  p2 <- indentLevel
  indentGT_ p2
  Let (first : rest) <$> pExpr
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
    var'      <- lowercaseName
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

pList :: Parser Syn
pList = ListLit <$> bracketsN (lexemeN pExpr `sepBy` lexemeN comma)

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
  pos  <- indentLevel
  alts <- some $ do
    indentGEQ_ pos
    pAlt
  spaceConsumerN
  pure $ MCase alts
 where
  pAlt :: Parser ([Syn.Pattern], Syn)
  pAlt = do
    pos  <- indentLevel
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
