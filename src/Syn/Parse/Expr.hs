module Syn.Parse.Expr
  ( pExpr
  , pBinApp
  , pApp
  , pVar
  , pMatch
  ) where

import           Control.Monad.Reader           ( asks )
import           Data.Functor                   ( void )
import qualified Data.List.NonEmpty            as NE

import qualified Control.Monad.Combinators.Expr
                                               as E
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           AST
import           Syn
import           Syn.Parse.Common
import           Syn.Parse.Pattern              ( pPattern )
import           Syn.Parse.Type                 ( pType )

pExpr :: Parser Syn
pExpr = do
  try pApp <|> try pBinApp <|> pExpr'

pExpr' :: Parser Syn
pExpr' = do
  pUnitLit
    <|> try pTuple
    <|> parensN pExpr
    <|> pHole
    <|> pCharLit
    <|> try pStringLit
    <|> try (IntLit <$> pInt)
    <|> try pRecordProject
    <|> pVar
    <|> pFCall
    <|> pLet
    <|> pRecordOrList
    <|> pCons
    <|> pMatch

-- | Compare our column to the current indent level,
-- and fail if we're not at least as indented as that.
ensureIndent :: Parser ()
ensureIndent = asks indentCol >>= indentGEQ_

pUnitLit :: Parser Syn
pUnitLit = do
  void $ symbolN "(,)"
  pure UnitLit

-- | Parse a binary application
-- The table is ordered by decreasing precedence.
-- As well as the usual arithmetic operators, we also parse function composition here.
-- TODO: However, function composition is not yet handled by the rest of the compiler.
-- We represent these operators as un-namespaced strings because they contain characters not valid
-- for user-defined names. It would be better to explicitly put them in the Kite.Prim module or
-- similar.
-- See the documentation for 'makeExprParser' for more info.
pBinApp :: Parser Syn
pBinApp = hang $ E.makeExprParser term table
 where
  -- Terms in a binary application can be normal applications or anything in 'pExpr'', because they
  -- are unambiguous. We don't allow unparenthesised multicases or nested binary applications.
  -- As with 'pExpr', we have to check the indent settings and fail if we don't satisfy them.
  term = ensureIndent >> (try pApp <|> pExpr')
  binary name = E.InfixL (App . App (Var (Name name)) <$ symbolN name)
  compose = E.InfixR (App . App (Var (Name ".")) <$ symbolN ".")
  -- We need to treat (-) specially because it shares a prefix with the comment delimiter "--"
  minus   = E.InfixL $ do
    -- Parse "-", unless it is directly followed by another "-"
    _ <- try $ lexemeN (string "-" >> notFollowedBy "-")
    pure (App . App (Var (Name "-")))
  table =
    [ [compose]
    , [binary "*", binary "/"]
    , [binary "+", minus, binary "<>"]
    , [binary "::"]
    , [binary ">=", binary "<=", binary ">", binary "<"]
    , [binary "&&", binary "||"]
    ]

-- | Parse two or more expressions, forming an application.
pApp :: Parser Syn
pApp = hang $ do
  first <- pExpr'
  rest  <- some pExpr'
  pure $ foldl App first rest

-- foo.bar
-- For ease of implementation we currently only support using projection on
-- variables. In the future we may want to support arbitrary expressions, e.g.
--   (let a = 1 in Foo { x = a }).x
pRecordProject :: Parser Syn
pRecordProject = do
  -- We don't use 'pVar' because we don't want to parse any trailing whitespace
  record <- lowercaseName
  void (string ".")
  field <- lexemeN lowercaseString
  pure $ Project (Var record) field

pHole :: Parser Syn
pHole = do
  void (string "?")
  name <- lexemeN pHoleName
  pure $ Hole name

pCharLit :: Parser Syn
pCharLit = CharLit <$> lexemeN pChar

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
pVar = Var <$> lexemeN lowercaseName

pFCall :: Parser Syn
pFCall = hang $ do
  void (symbolN "$fcall")
  name <- lexemeN lowercaseName
  args <- many pExpr'
  pure $ FCall (unName name) args

-- let foo = 1 { foo }
--
-- let foo = 1,
--     bar = 2,
--  { foo }
--
-- let foo = 1, bar = 2
--  { foo }
--
-- let foo : Int = 1,
--     bar : Bool -> Int = match {
--       True -> 1,
--       False -> 0,
--     }
--  { foo }
pLet :: Parser Syn
pLet = do
  void (symbolN "let")
  binds <- pBind `sepEndBy1` comma
  expr  <- braces pExpr
  pure $ Let binds expr
 where
  pBind :: Parser (RawName, Syn, Maybe Type)
  pBind = do
    var <- lowercaseName
    ty  <- optional (void (symbol ":") >> pType)
    void (symbolN "=")
    val <- pExpr
    pure (var, val, ty)

pTuple :: Parser Syn
pTuple = do
  _ <- symbolN "("
  hang $ do
    expr1 <- pExpr
    exprs <- some $ comma >> pExpr
    _     <- symbolN ")"
    pure $ TupleLit (expr1 : exprs)

pCons :: Parser Syn
pCons = do
  name <- lexemeN uppercaseName
  pure $ case name of
    "True"  -> BoolLit True
    "False" -> BoolLit False
    n       -> Con n

-- A 'match' expression.
-- If it has a target, it behaves like a 'case'.
-- Otherwise, it behaves like an 'mcase'.
--
-- match [1] {
--   [] -> Nothing,
--   x::_ -> Just x
-- }
--
-- head : [a] -> Maybe a {
--   match {
--     [] -> Nothing,
--     x::_ -> Just x
--   }
-- }
pMatch :: Parser Syn
pMatch = do
  void (symbol "match")
  maybeTarget <- optional pExpr
  case maybeTarget of
    Just target -> do
      alts <- braces $ pAlt `sepEndBy1` comma
      pure $ Case target alts
    Nothing -> do
      alts <- braces $ pMultiAlt `sepEndBy1` comma
      pure $ MCase alts
 where
  pAlt :: Parser (Syn.Pattern, Syn)
  pAlt = do
    pat <- pPattern
    void $ symbol "->"
    expr <- pExpr
    pure (pat, expr)
  pMultiAlt :: Parser ([Syn.Pattern], Syn)
  pMultiAlt = do
    pats <- pPattern `sepEndBy1` comma
    void $ symbolN "->"
    expr <- pExpr
    pure (pats, expr)

pRecordOrList :: Parser Syn
pRecordOrList =
  brackets
    $   do
          (Record <$> try pField `sepEndBy1` comma)
    <|> (ListLit <$> pExpr `sepEndBy` comma)
 where
  pField = do
    name <- lowercaseString
    void (symbol ":")
    expr <- pExpr
    pure (name, expr)
