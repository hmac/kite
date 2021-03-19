{-# LANGUAGE FlexibleContexts #-}
module Syn.Parse
  ( pModule
  , pFun
  , pExpr
  , parseKiteFile
  , pDecl
  , pType
  , pData
  , pImport
  , keywords
  , spaceConsumerN
  , Error
  , Parser
  ) where

import           Control.Monad                  ( guard )
import           Data.Functor                   ( void )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     ( indentLevel
                                                , nonIndented
                                                )


import           Syn                     hiding ( Pattern )

import           Syn.Parse.Common
import           Syn.Parse.Expr                 ( pExpr )
import           Syn.Parse.Type                 ( pConType
                                                , pType
                                                )

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
  nonIndented spaceConsumerN
    $   Comment
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
  p0       <- indentLevel
  name     <- lowercaseName <?> "function name"
  sig      <- symbol ":" >> pType
  indentGEQ_ p0
  _ <- lexeme lowercaseName >>= \n -> guard (name == n)
  void $ symbolN "="
  indentGT_ p0
  expr        <- pExpr
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

pComment :: Parser String
pComment = do
  void (string "-- " <|> string "--")
  s <- takeWhileP (Just "comment") (/= '\n')
  spaceConsumerN
  pure s
