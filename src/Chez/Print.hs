module Chez.Print where

-- Print a Chez Scheme AST as source code

import           Data.Text.Prettyprint.Doc
import           Prelude                 hiding ( print )

import           Chez                           ( Def(..)
                                                , Lit(..)
                                                , SExpr(..)
                                                )

printDef :: Def -> Doc a
printDef (Def name body) =
  parens $ "define" <+> pretty name <+> printSExpr body
printDef (DefRecord name fields) =
  parens $ "define-record-type" <+> pretty name <+> parens
    ("fields" <+> sepMap pretty fields)
printDef (DefFunc name args body) =
  parens $ "define" <+> parens (sepMap pretty (name : args)) <+> printSExpr body

printSExpr :: SExpr -> Doc a
printSExpr = \case
  Quote e    -> "'" <> printSExpr e
  Lit   l    -> printLit l
  List  xs   -> parens $ sepMap printSExpr xs
  Vec   xs   -> "'#" <> parens (sepMap printSExpr xs)
  Var   v    -> pretty v
  App f args -> parens $ printSExpr f <+> sepMap printSExpr args
  Abs vars body ->
    parens $ "lambda" <+> parens (sepMap pretty vars) <+> printSExpr body
  Let binds body ->
    parens
      $   "letrec*"
      <+> parens
            (sepMap (\(v, e) -> parens (sep [pretty v, printSExpr e])) binds)
      <+> printSExpr body
  Cond alts ->
    parens
      $   "cond"
      <+> sepMap (\(c, e) -> parens (sepMap printSExpr [c, e])) alts

printLit :: Lit -> Doc a
printLit = \case
  Bool   True  -> "#t"
  Bool   False -> "#f"
  Int    n     -> pretty n
  Char   c     -> "#\\" <> pretty c
  String s     -> dquotes (pretty s)
  Unit         -> "'()"

sepMap :: (a -> Doc b) -> [a] -> Doc b
sepMap f = sep . map f
