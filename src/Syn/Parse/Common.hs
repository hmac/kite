-- Shared types and functions for parsing the surface syntax
module Syn.Parse.Common where

import           Control.Monad                  ( guard )
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Data.Maybe                     ( fromMaybe )

import           Data.Name                      ( PackageName
                                                , mkPackageName
                                                )
import           Syn                            ( ModuleName(ModuleName)
                                                , RawName(Name)
                                                )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     ( indentGuard )
import qualified Text.Megaparsec.Char.Lexer    as L


type Parser = Parsec Error String

data Error
  = VarKeyword String
  | InvalidPackageName String
  deriving (Eq, Ord, Show)

instance ShowErrorComponent Error where
  showErrorComponent = \case
    VarKeyword v ->
      show v <> " is a reserved keyword and cannot be used as a variable name."
    InvalidPackageName n -> show n <> " is not a valid package name."

pChar :: Parser Char
pChar = between (string "'") (symbol "'") $ escapedChar <|> fmap
  head
  (takeP (Just "char") 1)
  -- Escaped special characters like \n
  -- Currently we only support \n
  -- TODO: what's the full list of escape sequences we should support here?
  where escapedChar = char '\\' >> char 'n' $> '\n'

pInt :: Parser Int
pInt = do
  sign   <- optional (string "-")
  digits <- lexeme (some digitChar)
  spaceConsumerN
  pure . read $ fromMaybe "" sign <> digits

pName :: Parser RawName
pName = uppercaseName <|> lowercaseName

pModuleName :: Parser ModuleName
pModuleName = ModuleName <$> lexeme (uppercaseString' `sepBy` string ".")
 where
  -- like uppercaseString but doesn't consume trailing space
  uppercaseString' :: Parser String
  uppercaseString' = (:) <$> upperChar <*> many alphaNumChar

pPackageName :: Parser PackageName
pPackageName = try $ do
  s <- lowercaseString
  case mkPackageName s of
    Just n  -> pure n
    Nothing -> customFailure (InvalidPackageName s)


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

-- TODO: some of these can be used in expressions, e.g. from, qualified, as, module, import
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
