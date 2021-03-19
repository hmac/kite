module Syn.Parse.Pattern (pPattern, pCasePattern) where

import           Text.Megaparsec

import Syn.Parse.Common
import Syn (RawName(Name))
import qualified Syn
import AST (Pat(..))


pPattern :: Parser Syn.Pattern
pPattern = try pPattern' <|> pConPat True

-- TODO: merge with pPattern?
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

