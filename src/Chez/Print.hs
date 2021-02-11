module Chez.Print where

-- Print a Chez Scheme AST as source code

import           Prelude                 hiding ( print )
import           Data.Text.Prettyprint.Doc

import           Chez                           ( SExpr(..)
                                                , Lit(..)
                                                , Def(..)
                                                )

printDef :: Def -> Doc a
printDef (Def name body) =
  parens $ "define" <+> pretty name <+> printSExpr body
printDef (DefRecord name fields) =
  parens $ "define-record-type" <+> pretty name <+> parens
    ("fields" <+> sepMap pretty fields)

printSExpr :: SExpr -> Doc a
printSExpr = \case
  Lit  l     -> printLit l
  List xs    -> parens $ "list" <+> sepMap printSExpr xs
  Var  v     -> pretty v
  App f args -> parens $ printSExpr f <+> sepMap printSExpr args
  Abs vars body ->
    parens $ "lambda" <+> parens (sepMap pretty vars) <+> printSExpr body
  Let binds body ->
    parens
      $   "letrec"
      <+> parens
            (sepMap (\(v, e) -> parens (sep [pretty v, printSExpr e])) binds)
      <+> printSExpr body
  If c t e -> parens $ "if" <+> printSExpr c <+> printSExpr t <+> printSExpr e
  Cond alts ->
    parens
      $   "cond"
      <+> sepMap (\(c, e) -> parens (sepMap printSExpr [c, e])) alts

printLit :: Lit -> Doc a
printLit = \case
  Bool   True  -> "#t"
  Bool   False -> "#f"
  Int    n     -> pretty n
  Char   c     -> squotes (pretty c)
  String s     -> dquotes (pretty s)
  Unit         -> "'()"

sepMap :: (a -> Doc b) -> [a] -> Doc b
sepMap f = sep . map f
