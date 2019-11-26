{-# LANGUAGE OverloadedStrings #-}
module Print where

import           Data.List                      ( intersperse )
import           Data.Text.Prettyprint.Doc
import           Prelude                 hiding ( mod )

import           Syntax

-- TODO: we need to parse and then print comments, too
printModule :: Module -> Doc a
printModule mod = vsep
  [ printMetadata (moduleMetadata mod)
  , printModName (moduleName mod)
  , indent 2 (printModExports (moduleExports mod))
  , line
  , vsep (map printModImports (moduleImports mod))
  , line
  , vsep (intersperse mempty (map printDecl (moduleDecls mod)))
  ]

-- ---
-- key1: val1
-- key2: val2
-- ---
printMetadata :: [(String, String)] -> Doc a
printMetadata kvs = vsep ["---", printKvs, "---"]
  where printKvs = vsep $ map (\(k, v) -> hcat [pretty k, ": ", pretty v]) kvs

-- module Foo
printModName :: ModuleName -> Doc a
printModName (ModuleName names) =
  "module" <+> hcat (map pretty (intersperse "." names))

--   (fun1, fun2)
printModExports :: [Name] -> Doc a
printModExports []      = mempty
printModExports exports = tupled (map printName exports)

-- import Data.Text
-- import qualified Data.Text.Encoding as E (encodeUtf8)
printModImports :: Import -> Doc a
printModImports i = hsep
  [ "import"
  , if importQualified i then "qualified" else "         "
  , prettyModuleName (importName i)
  , maybe mempty (\n -> "as" <+> printName n) (importAlias i)
  , tupled (map printName (importItems i))
  ]

-- TODO
printDecl :: Decl -> Doc a
printDecl (FunDecl       f) = printFun f
printDecl (DataDecl      d) = printData d
printDecl (TypeclassDecl t) = printTypeclass t
printDecl (TypeclassInst i) = printInstance i

printFun :: Fun -> Doc a
printFun Fun { funName = name, funDefs = defs, funType = ty } =
  vsep $ sig : map (printDef name) defs
  where sig = printName name <+> colon <+> printType ty

-- we special case a top level TyArr by not wrapping it in parens
-- because we want foo : (a -> b) -> c
-- rather than foo : ((a -> b) -> c)
printType :: Ty -> Doc a
printType t = case t of
  TyArr _ _ -> hsep $ intersperse "->" (map printType' (unfoldTyApp t))
  ty        -> printType' ty
 where
  printType' (TyVar n    ) = printName n
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

-- TODO: correct parentheses for applications etc.
printExpr :: Syn -> Doc a
printExpr (Var  n) = printName n
printExpr (Cons n) = printName n
printExpr (Abs args e) =
  parens $ "\\" <> hsep (map printName args) <+> "->" <+> printExpr e
printExpr (App  a     b   ) = printApp a b
printExpr (Let  binds e   ) = printLet binds e
printExpr (Case e     alts) = printCase e alts
printExpr (TupleLit es    ) = tupled (map printExpr es)
printExpr (ListLit es) | any big es = printList es
                       | otherwise  = list (map printExpr es)
printExpr (Lit l) = printLiteral l

printApp :: Syn -> Syn -> Doc a
printApp a b = printExpr a <+> printExpr b

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
  printName (dataName d)
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
    indent 2 $ printName name <+> equals <+> printType ty

printInstance :: Instance -> Doc a
printInstance i = vsep (header : map printInstanceDef (instanceDefs i))
 where
  header = "instance" <+> printName (instanceName i) <+> hsep
    (map printType (instanceTypes i))
  printInstanceDef (name, defs) = indent 2 $ vsep (map (printDef name) defs)

printName :: Name -> Doc a
printName (Name n) = pretty n

prettyModuleName :: ModuleName -> Doc a
prettyModuleName (ModuleName names) = hcat (map pretty (intersperse "." names))

big :: Syn -> Bool
big (Case _ _  ) = True
big (Let  _ _  ) = True
big (App  a b  ) = size (App a b) > 5
big (ListLit xs) = any big xs
big _            = False

size :: Syn -> Int
size (App a b) = size a + size b
size _         = 1
