module Syn.Parse.Expr
  ( pExpr
  , pBinApp
  , pApp
  , pMultiCase
  , pCase
  , pVar
  ) where

import           Control.Monad                  ( guard )
import           Control.Monad.Reader           ( asks )
import           Data.Functor                   ( void )
import qualified Data.List.NonEmpty            as NE

import qualified Control.Monad.Combinators.Expr
                                               as E
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
pExpr = do
  ensureIndent
  try pIAbs <|> try pMultiCase <|> try pApp <|> try pBinApp <|> pExpr'

pExpr' :: Parser Syn
pExpr' = do
  ensureIndent

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
    <|> pLet
    <|> pList
    <|> pCons
    <|> pCase

-- | Parse an expression which is permitted inside a string interpolation.
-- This is restricted to expressions that can be represented as a single token.
pInterpExpr :: Parser Syn
pInterpExpr = do
  ensureIndent
  pUnitLit <|> pHole <|> pCharLit <|> try (IntLit <$> pInt) <|> pVar <|> pCons

-- | Compare our column to the current indent level,
-- and fail if we're not at least as indented as that.
ensureIndent :: Parser ()
ensureIndent = asks indentCol >>= indentGEQ_

pUnitLit :: Parser Syn
pUnitLit = do
  void $ symbolN "()"
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
        e <- pInterpExpr
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
pLet = hang $ do
  void (symbolN "let")
  binds <- hang $ some (pAnnotatedBind <|> pBind)
  expr  <- symbolN "in" >> hang pExpr
  pure $ Let binds expr
 where
  pBind :: Parser (RawName, Syn, Maybe Type)
  pBind = do
    var <- lowercaseName
    void (symbolN "=")
    val <- hang $ lexemeN pExpr
    pure (var, val, Nothing)
  pAnnotatedBind :: Parser (RawName, Syn, Maybe Type)
  pAnnotatedBind = do
    (var, ty) <- try pAnnotation
    var'      <- lowercaseName
    guard (var' == var)
    void (symbolN "=")
    val <- hang $ lexemeN pExpr
    pure (var, val, Just ty)
  pAnnotation :: Parser (RawName, Type)
  pAnnotation = do
    var <- lowercaseName
    void (symbolN ":")
    ty <- lexemeN pType
    pure (var, ty)

pTuple :: Parser Syn
pTuple = do
  _ <- symbolN "("
  hang $ do
    expr1 <- pExpr
    exprs <- some $ comma >> pExpr
    _     <- symbolN ")"
    pure $ TupleLit (expr1 : exprs)

pList :: Parser Syn
pList = ListLit <$> bracketsN (lexemeN pExpr `sepBy` lexemeN comma)

pCons :: Parser Syn
pCons = do
  name <- lexemeN uppercaseName
  pure $ case name of
    "True"  -> BoolLit True
    "False" -> BoolLit False
    n       -> Con n

-- We use 'indentGT_' and 'indentLevel' instead of 'hang' here because we want case alts to be
-- indent at least two columns beyond the "c" of "case", and 'hang' doesn't really support that at
-- the moment.
pCase :: Parser Syn
pCase = do
  pos <- indentLevel
  void (symbol "case ")
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
    void (string "->")
    expr <- hang (spaceConsumerN >> pExpr)
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
--   or equivalently:
--
--   let f True [] = 1
--       f _    _  = 2
--    in f
--
-- Note that constructor patterns must be parenthesised to distinguish them from
-- multiple independent patterns. This is why we use pPattern rather than
-- pCasePattern.
pMultiCase :: Parser Syn
pMultiCase = MCase <$> hang (some pAlt)
 where
  pAlt :: Parser ([Syn.Pattern], Syn)
  pAlt = do
    ensureIndent
    pos  <- indentLevel
    pats <- some $ do
      -- Patterns must be equally or more indented than the start of the mcase
      -- This prevents us from parsing any futher alts as part of this alt.
      indentGEQ_ pos
      pPattern
    void (symbolN "->")
    -- The RHS of an alt must be indented more than the alt pattern.
    indentGT_ pos
    expr <- hang pExpr
    pure (pats, expr)

-- | An 'IAbs' is like a single-parameter multi-case which binds an
-- implicit parameter.
--
--     f : A => B
--     f = a => b
pIAbs :: Parser Syn
pIAbs = do
  ensureIndent
  pos <- indentLevel
  pat <- pPattern
  void (symbolN "=>")
  indentGT_ pos
  expr <- hang pExpr
  pure $ IAbs pat expr

pRecord :: Parser Syn
pRecord = Record <$> bracesN (pField `sepBy1` symbolN ",")
 where
  pField = do
    name <- lowercaseString
    void (symbol "=")
    expr <- pExpr
    pure (name, expr)
