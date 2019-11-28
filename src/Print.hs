{-# LANGUAGE OverloadedStrings #-}
module Print where

import           Data.Maybe                     ( catMaybes )
import           Data.List                      ( intersperse )
import           Data.Text.Prettyprint.Doc
import           Prelude                 hiding ( mod )

import           Syntax

-- TODO: we need to parse and then print comments, too
printModule :: Module -> Doc a
printModule mod = vsep $ catMaybes
  [ printMetadata (moduleMetadata mod)
  , Just $ printModName (moduleName mod)
  , indent 2 <$> printModExports (moduleExports mod)
  , Just mempty
  , printImports (moduleImports mod)
  , Just mempty
  , printModDecls (moduleDecls mod)
  ]

-- ---
-- key1: val1
-- key2: val2
-- ---
-- The return type is a Maybe so we can render the empty string when there's no
-- metadata. (https://stackoverflow.com/a/52843774)
printMetadata :: [(String, String)] -> Maybe (Doc a)
printMetadata []  = Nothing
printMetadata kvs = Just $ vsep ["---", printKvs, "---"]
  where printKvs = vsep $ map (\(k, v) -> hcat [pretty k, ": ", pretty v]) kvs

-- module Foo
printModName :: ModuleName -> Doc a
printModName (ModuleName names) =
  "module" <+> hcat (map pretty (intersperse "." names))

--   (fun1, fun2)
printModExports :: [Name] -> Maybe (Doc a)
printModExports []      = Nothing
printModExports exports = Just $ tupled (map printName exports)

-- import Data.Text
-- import qualified Data.Text.Encoding as E (encodeUtf8)
printImports :: [Import] -> Maybe (Doc a)
printImports []      = Nothing
printImports imports = Just $ vsep (map printImport imports)

printImport :: Import -> Doc a
printImport i = hsep
  [ "import"
  , if importQualified i then "qualified" else "         "
  , prettyModuleName (importName i)
  , maybe mempty (\n -> "as" <+> printName n) (importAlias i)
  , tupled (map printName (importItems i))
  ]

printModDecls :: [Decl] -> Maybe (Doc a)
printModDecls []    = Nothing
printModDecls decls = Just $ vsep (intersperse mempty (map printDecl decls))

printDecl :: Decl -> Doc a
printDecl (Comment       c) = printComment c
printDecl (FunDecl       f) = printFun f
printDecl (DataDecl      d) = printData d
printDecl (TypeclassDecl t) = printTypeclass t
printDecl (TypeclassInst i) = printInstance i

printFun :: Fun -> Doc a
printFun Fun { funComments = comments, funName = name, funDefs = defs, funType = ty }
  = vsep $ printComments comments ++ [sig] ++ map (printDef name) defs
 where
  sig = printName name <> align (space <> colon <+> printType ty)
  printComments [] = []
  printComments cs = map printComment cs

-- we special case a top level TyArr by not wrapping it in parens
-- because we want foo : (a -> b) -> c
-- rather than foo : ((a -> b) -> c)
printType :: Ty -> Doc a
printType t = case t of
  TyArr _ _ -> sep $ intersperse "->" (map printType' (unfoldTyApp t))
  ty        -> printType' ty
 where
  printType' (TyHole n   ) = "?" <> printName n
  printType' (TyVar  n   ) = printName n
  printType' (TyApp n tys) = printName n <+> hsep (map printType' tys)
  printType' ty@(TyArr _ _) =
    parens $ hsep $ intersperse "->" (map printType' (unfoldTyApp ty))
  printType' (TyList  ty) = brackets (printType' ty)
  printType' (TyTuple ts) = tupled (map printType' ts)

-- Unwrap a nested TyArr tree so we can print a -> b -> c
-- rather than a -> (b -> c)
unfoldTyApp :: Ty -> [Ty]
unfoldTyApp t = reverse (go t [])
 where
  go (TyArr a b) ts = go b (a : ts)
  go ty          ts = ty : ts

-- For "big" expressions, print them on a new line under the =
-- For small expressions, print them on the same line
printDef :: Name -> Def -> Doc a
printDef name d | big (defExpr d) = nest 2 $ vsep [lhs, printExpr (defExpr d)]
                | otherwise       = lhs <+> printExpr (defExpr d)
  where lhs = printName name <+> hsep (map printPattern (defArgs d)) <+> equals

printPattern :: Pattern -> Doc a
printPattern (VarPat n)      = printName n
printPattern WildPat         = "_"
printPattern (LitPat   l   ) = printLiteral l
printPattern (TuplePat pats) = tupled (map printPattern pats)
printPattern (ListPat  pats) = list (map printPattern pats)
printPattern (ConsPat n pats) =
  parens $ printName n <+> hsep (map printPattern pats)

printLiteral :: Literal -> Doc a
printLiteral (LitInt    i) = pretty i
printLiteral (LitFloat  f) = pretty f
printLiteral (LitString s) = dquotes (pretty s)

-- TODO: binary operators
printExpr :: Syn -> Doc a
printExpr (Var  n) = printName n
printExpr (Cons n) = printName n
printExpr (Hole n) = "?" <> printName n
printExpr (Abs args e) =
  parens $ "\\" <> hsep (map printName args) <+> "->" <+> printExpr e
printExpr (App  a     b   ) = printApp a b
printExpr (Let  binds e   ) = printLet binds e
printExpr (Case e     alts) = printCase e alts
printExpr (TupleLit es    ) = tupled (map printExpr es)
printExpr (ListLit es) | any big es = printList es
                       | otherwise  = list (map printExpr es)
printExpr (Lit l) = printLiteral l

-- Can we simplify this by introducting printExpr' which behaves like printExpr
-- but always parenthesises applications?
printApp :: Syn -> Syn -> Doc a
printApp (App (Var op) a) b | op `elem` binOps =
  parens $ case (singleton a, singleton b) of
    (True, True) -> printExpr a <+> printExpr (Var op) <+> printExpr b
    (False, False) ->
      parens (printExpr a) <+> printExpr (Var op) <+> parens (printExpr b)
    (True, False) ->
      printExpr a <+> printExpr (Var op) <+> parens (printExpr b)
    (False, True) ->
      parens (printExpr a) <+> printExpr (Var op) <+> printExpr b
printApp a (App b c) = if big a
  then parens (printExpr a) <+> nest 2 (parens (printExpr (App b c)))
  else printExpr a <+> parens (printExpr (App b c))
printApp a b | big a && big b =
  parens (printExpr a) <+> nest 2 (parens (printExpr b))
printApp a b | big a = parens (printExpr a) <+> nest 2 (printExpr b)
printApp a b | big b = printExpr a <+> parens (printExpr b)
printApp a b         = printExpr a <+> printExpr b

-- Used if the list is 'big'
printList :: [Syn] -> Doc a
printList es = nest
  2
  (encloseSep (lbracket <> line) (line <> rbracket) comma (map printExpr es))

-- let x = 1
--     y = 2
--  in expr
--
--  The hang (-3) pushes 'in' back to end in line with 'let'
printLet :: [(Name, Syn)] -> Syn -> Doc a
printLet binds e = "let" <+> hang
  (-3)
  (vsep [hang 0 (vsep (map printLetBind binds)), "in" <+> printExpr e])
  where printLetBind (name, expr) = printName name <+> "=" <+> printExpr expr

-- case expr of
--   pat1 x y -> e1
--   pat2 z w -> e2
printCase :: Syn -> [(Pattern, Syn)] -> Doc a
printCase e alts = "case"
  <+> hang (-3) (vsep ((printExpr e <+> "of") : map printAlt alts))
  where printAlt (pat, expr) = printPattern pat <+> "->" <+> printExpr expr

printData :: Data -> Doc a
printData d =
  "data"
    <+> printName (dataName d)
    <+> hsep (map printName (dataTyVars d))
    <+> equals
    <+> hsep (punctuate pipe (map printCon (dataCons d)))

printCon :: DataCon -> Doc a
printCon c = printName (conName c) <+> hsep (map printType (conArgs c))

printTypeclass :: Typeclass -> Doc a
printTypeclass t = vsep (header : map printTypeclassDef (typeclassDefs t))
 where
  header = "class" <+> printName (typeclassName t) <+> hsep
    (map printName (typeclassTyVars t))
  printTypeclassDef (name, ty) =
    indent 2 $ printName name <+> colon <+> printType ty

printInstance :: Instance -> Doc a
printInstance i = vsep (header : map printInstanceDef (instanceDefs i))
 where
  header = "instance" <+> printName (instanceName i) <+> hsep
    (map printType (instanceTypes i))
  printInstanceDef (name, defs) = indent 2 $ vsep (map (printDef name) defs)

printComment :: String -> Doc a
printComment c = "--" <+> pretty c

printName :: Name -> Doc a
printName (Name n) = pretty n

prettyModuleName :: ModuleName -> Doc a
prettyModuleName (ModuleName names) = hcat (map pretty (intersperse "." names))

-- True if we would never need to parenthesise it
singleton :: Syn -> Bool
singleton (Var      _) = True
singleton (Hole     _) = True
singleton (Cons     _) = True
singleton (Lit      _) = True
singleton (TupleLit _) = True
singleton (ListLit  _) = True
singleton _            = False

big :: Syn -> Bool
big (Case _ _  ) = True
big (Let  _ _  ) = True
big (App  a b  ) = size (App a b) > 5
big (ListLit xs) = any big xs
big _            = False

size :: Syn -> Int
size (App a b) = size a + size b
size _         = 1
