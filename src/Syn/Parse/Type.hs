module Syn.Parse.Type
  ( pType
  , pConType
  ) where

import           Data.Functor                   ( void )

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Syn
import           Syn.Parse.Common

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

-- When parsing the type args to a constructor, type application must be inside
-- parentheses
-- i.e. MyCon f a   -> name = MyCon, args = [f, a]
-- vs.  func : f a  -> name = func, type = f :@: a
pConType :: Parser Type
pConType = pType' Paren

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
