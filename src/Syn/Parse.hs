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
  , parse
  ) where

import           Data.Functor                   ( void )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )

import           Text.Megaparsec         hiding ( parse )
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     ( nonIndented )


import           Syn                     hiding ( Pattern )

import           Data.Name                      ( PkgModuleName(..) )
import           Syn.Parse.Common
import           Syn.Parse.Expr                 ( pExpr )
import           Syn.Parse.Type                 ( pConType
                                                , pType
                                                )

-- TODO: markdown in comments & doctests
-- TODO: heredocs
-- TODO: do notation
-- TODO: record patterns
-- TODO: infix constructors (like List ::)
-- TODO: empty data types (e.g. Void)


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

parseKiteFile :: FilePath -> PackageName -> String -> Either String Module
parseKiteFile path pkgName = parse (pModule pkgName <* eof) path

pModule :: PackageName -> Parser Module
pModule pkgName = do
  void $ symbol "module"
  name    <- lexemeN pModuleName
  exports <- optional . braces $ pExport `sepBy` comma
  imports <- many (pImport pkgName)
  decls   <- many pDecl
  pure $ Module { moduleName     = PkgModuleName pkgName name
                , moduleImports  = imports
                , moduleExports  = fromMaybe [] exports
                , moduleDecls    = decls
                , moduleMetadata = []
                }

-- Foo { Bar, Baz }
-- mkFoo
-- TODO: Maybe {*}
pExport :: Parser (RawName, [RawName])
pExport = do
  export     <- pName
  subexports <- fromMaybe [] <$> optional (braces (pName `sepBy` comma))
  pure (export, subexports)

-- import Bar
-- import Baz {fun1, fun2} as Boo 
-- import Foo {SomeType{ * }, OtherType{ AConstructor }, SomeClass}
-- import qualified Data.Show
-- from some_pkg import Foo
-- TODO: import Foo { * }
pImport :: PackageName -> Parser Import
pImport selfPkg = do
  pkg <- optional $ string "from " >> pPackageName
  void $ symbol "import"
  qualified <- isJust <$> optional (symbol "qualified")
  name      <- pModuleName
  items     <- optional $ braces (pImportItem `sepBy` comma)
  alias     <- optional (symbol "as" >> uppercaseName)
  pure Import { importQualified = qualified
              , importName      = PkgModuleName (fromMaybe selfPkg pkg) name
              , importAlias     = alias
              , importItems     = fromMaybe [] items
              }

pImportItem :: Parser ImportItem
pImportItem = try pImportAll <|> try pImportSome <|> pImportSingle
 where
  -- Foo{ * }
  pImportAll = do
    name <- lexemeN uppercaseName
    void $ braces (symbol "*")
    pure $ ImportAll name
  -- Foo{ Bar, Baz }
  -- Monoid{ empty }
  pImportSome = do
    name     <- lexemeN uppercaseName
    subItems <- braces (pName `sepBy` comma)
    pure $ ImportSome name subItems
  -- Foo
  -- foo
  pImportSingle = ImportSingle <$> pName

-- We want comments above functions to be associated with them, but doing this
-- in the parser leads to some backtracking that worsens error messages and
-- reduces performance, so we keep it simple here. In a later stage of the
-- compiler we merge adjacent comment and function declarations.
pDecl :: Parser (Decl Syn)
pDecl = Comment <$> pComment <|> nonIndented
  spaceConsumerN
  (AliasDecl <$> pAlias <|> DataDecl <$> pData <|> FunDecl <$> pFun)

-- type alias FallibleFn a b { a -> Maybe b }
pAlias :: Parser Alias
pAlias = do
  void (symbolN "type alias ")
  alias  <- uppercaseName
  tyvars <- many lowercaseName
  ty     <- braces pType
  pure Alias { aliasName = alias, aliasTyVars = tyvars, aliasType = ty }

-- type Maybe a { Just a, Nothing }
pData :: Parser Data
pData = do
  void (symbolN "type ")
  name         <- uppercaseName
  tyvars       <- many lowercaseName
  constructors <- braces $ pCon `sepBy` comma
  pure Data { dataName = name, dataTyVars = tyvars, dataCons = constructors }
 where
  pCon :: Parser DataCon
  pCon = DataCon <$> uppercaseName <*> many pConType

-- Parse a function definition
-- Functions consist of a type signature, a newline, and an implementation
-- Unlike Haskell, functions only have a single equation.
-- Pattern matching happens on the RHS
--
-- swap : (a, b) -> (b, a) {
--  match {
--    (x, y) -> (y, x)
--  }
-- }
pFun :: Parser (Fun Syn)
pFun = do
  comments <- many pComment
  name     <- lowercaseName <?> "function name"
  _        <- symbol ":"
  sig      <- pType
  expr     <- braces pExpr

  pure Fun { funComments = comments
           , funName     = name
           , funType     = Just sig
           , funExpr     = expr
           , funWheres   = []
           }

pComment :: Parser String
pComment = do
  void (string "# " <|> string "#")
  s <- takeWhileP (Just "comment") (/= '\n')
  spaceConsumerN
  pure s
