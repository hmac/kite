module Syn.Print where

import           Data.List                      ( intersperse )
import qualified Data.List.NonEmpty            as NE
import           Prelude                 hiding ( mod )
import           Util

import           Data.Text.Prettyprint.Doc

import           AST
import           Data.Name                      ( PkgModuleName(..) )
import           Print                          ( Document
                                                , data_
                                                , func
                                                , hole
                                                , keyword
                                                , type_
                                                )
import qualified Syn
import           Syn                     hiding ( Pattern )

printModule :: Module -> Document
printModule mod = vsep $ catMaybes
  [ printMetadata (moduleMetadata mod)
  , Just $ printModName modName
  , indent 2 <$> printModExports (moduleExports mod)
  , (line <>) <$> printImports pkgName (moduleImports mod)
  , (line <>) <$> printModDecls (moduleDecls mod)
  ]
  where PkgModuleName pkgName modName = moduleName mod

-- ---
-- key1: val1
-- key2: val2
-- ---
-- The return type is a Maybe so we can render the empty string when there's no
-- metadata. (https://stackoverflow.com/a/52843774)
printMetadata :: [(String, String)] -> Maybe Document
printMetadata []  = Nothing
printMetadata kvs = Just $ vsep ["---", printKvs, "---"]
  where printKvs = vsep $ map (\(k, v) -> hcat [pretty k, ": ", pretty v]) kvs

-- module Foo
printModName :: ModuleName -> Document
printModName (ModuleName names) =
  keyword "module" <+> hcat (map pretty (intersperse "." names))

--   (fun1, fun2)
printModExports :: [(RawName, [RawName])] -> Maybe Document
printModExports []      = Nothing
printModExports exports = Just $ tupled (map printExport exports)
 where
  printExport :: (RawName, [RawName]) -> Document
  printExport (e, []) = printName e
  printExport (e, s ) = printName e <> tupled (map printName s)

-- import Data.Text
-- import qualified Data.Text.Encoding as E (encodeUtf8)
printImports :: PackageName -> [Import] -> Maybe Document
printImports _       []      = Nothing
printImports selfPkg imports = Just $ vsep (map (printImport selfPkg) imports)

printImport :: PackageName -> Import -> Document
printImport selfPkg i =
  let
    PkgModuleName pkgName modName = importName i
    pkg                           = if pkgName == selfPkg
      then mempty
      else keyword "from" <+> printPackageName pkgName <> " "
    qual  = if importQualified i then keyword " qualified " else "           "
    alias = maybe mempty (\n -> keyword " as" <+> printName n) (importAlias i)
    items = if null (importItems i)
      then mempty
      else " " <> hang 0 (tupled (map printImportItem (importItems i)))
  in
    hcat [pkg, keyword "import", qual, prettyModuleName modName, alias, items]

printImportItem :: ImportItem -> Document
printImportItem i = printName (importItemName i) <> case i of
  ImportSingle{} -> mempty
  ImportAll{}    -> parens ".."
  ImportSome{}   -> tupled (map printName (importItemConstructors i))

printModDecls :: [Decl Syn] -> Maybe Document
printModDecls []    = Nothing
printModDecls decls = Just $ vsep (intersperse mempty (map printDecl decls))

printDecl :: Decl Syn -> Document
printDecl (Comment   c) = printComment c
printDecl (FunDecl   f) = printFun f
printDecl (DataDecl  d) = printData d
printDecl (AliasDecl a) = printAlias a

printFun :: Fun Syn -> Document
printFun Fun { funComments = comments, funName = name, funExpr = defs, funType = ty, funWheres = wheres }
  = vsep $ printComments comments ++ sig ++ [printDef name defs] ++ whereClause
 where
  whereClause = case wheres of
    [] -> []
    ws -> [hang 2 $ forceVSep $ " where" : map printFun ws]
  sig = case ty of
    Just t  -> [func (printName name) <> align (space <> colon <+> printType t)]
    Nothing -> []
  printComments [] = []
  printComments cs = map printComment cs

-- a -> b
-- f a -> f b
-- a -> b -> c
-- (a -> b) -> c
printType :: Type -> Document
printType = printType' Root

data Context = Root | AppL | AppR  | ArrL | ArrR

printType' :: Context -> Type -> Document
printType' ctx ty = case (ctx, ty) of
  -- top level arrows don't get parenthesised
  (Root, TyFun a b      ) -> printType' ArrL a <+> "->" <+> printType' ArrR b
  -- applications of TyList get special-cased
  (Root, TyApp TyList a ) -> brackets (printType' Root a)
  -- top level applications don't get parenthesised
  (Root, TyApp a b      ) -> printType' AppL a <+> printType' AppR b

  -- arrows on the left of arrows get parenthesised
  (ArrL, TyFun a b      ) -> parens $ printType' Root (a `fn` b)
  -- arrows on the right of arrows don't
  (ArrR, TyFun a b      ) -> printType' Root (a `fn` b)
  -- arrows on either side of applications get parenthesised
  (AppR, TyFun a b      ) -> parens $ printType' Root (a `fn` b)
  (AppL, TyFun a b      ) -> parens $ printType' Root (a `fn` b)

  -- applications on the left of applications don't get parenthesised
  (AppL, t@TyApp{}      ) -> printType' Root t
  -- applications on the right of applications get parenthesised
  (AppR, t@TyApp{}      ) -> parens $ printType' Root t
  -- applications on either side of arrows don't
  (ArrL, t@TyApp{}      ) -> printType' Root t
  (ArrR, t@TyApp{}      ) -> printType' Root t

  -- Basic cases
  (_   , TyCon n        ) -> type_ $ printName n
  -- Type aliases are treated like constructors
  (_   , TyAlias alias _) -> type_ $ printName alias
  (_   , TyHole n       ) -> hole ("?" <> printName n)
  (_   , TyVar n        ) -> printName n
  (_   , TyList         ) -> type_ "[]"
  (_   , TyTuple ts     ) -> tupled (map (printType' Root) ts)
  (_   , TyInt          ) -> type_ "Int"
  (_   , TyString       ) -> type_ "String"
  (_   , TyChar         ) -> type_ "Char"
  (_   , TyBool         ) -> type_ "Bool"
  (_   , TyUnit         ) -> type_ "()"
  (_, TyRecord fields) ->
    printRecordSyntax ":" $ map (bimap printName printType) fields
  (_, TyForall v t) -> "forall" <+> printName v <> "." <+> printType t

-- For "big" expressions, print them on a new line under the =
-- For small expressions, print them on the same line
-- TODO: when we drop support for LHS patterns, update this
printDef :: RawName -> Syn -> Document
printDef name expr | big expr  = nest 2 $ vsep [lhs, printExpr expr]
                   | otherwise = lhs <+> printExpr expr
  where lhs = func (printName name) <+> equals

printPattern :: Syn.Pattern -> Document
printPattern (VarPat n)      = printName n
printPattern WildPat         = "_"
printPattern (IntPat    i)   = pretty i
printPattern (CharPat   c)   = squotes (pretty c)
printPattern (StringPat s)   = dquotes (pretty s)
printPattern (BoolPat   b)   = pretty b
printPattern UnitPat         = "()"
printPattern (TuplePat pats) = align $ htupled (map printPattern pats)
printPattern (ListPat  pats) = list (map printPattern pats)
-- special case for the only infix constructor: (::)
printPattern (ConsPat "::" _ [x, y]) =
  parens $ printPattern x <+> "::" <+> printPattern y
printPattern (ConsPat n _ pats) =
  parens $ data_ (printName n) <+> hsep (map printPattern pats)

-- TODO: binary operators
printExpr :: Syn -> Document
printExpr (Var n  ) = printName n
printExpr (Ann e t) = printExpr e <+> colon <+> printType t
printExpr (Con n  ) = data_ (printName n)
printExpr (Record fields) =
  data_ $ printRecordSyntax "=" $ map (bimap pretty printExpr) fields
printExpr (Project e f) = printExpr' e <> dot <> pretty f
printExpr (Hole n     ) = hole ("?" <> printName n)
printExpr (Abs args e) =
  parens $ "\\" <> hsep (map printName args) <+> "->" <+> printExpr e
printExpr (App  a     b   ) = printApp a b
printExpr (Let  binds e   ) = printLet binds e
printExpr (Case e     alts) = printCase e alts
printExpr (MCase    alts  ) = printMCase alts
printExpr (TupleLit es    ) = align $ tupled (map printExpr es)
printExpr (ListLit  es    ) = printList es
printExpr (IntLit   i     ) = pretty i
printExpr (StringInterp prefix interps) =
  printInterpolatedString prefix (NE.toList interps)
printExpr (StringLit s    ) = printInterpolatedString s []
-- We use Haskell's encoding rules for characters like \NUL by just deferring to
-- the Show instance for Char. This is mostly for convenience and we may want to define our own
-- rules in the future.
printExpr (CharLit   c    ) = pretty (show c)
printExpr (BoolLit   True ) = data_ "True"
printExpr (BoolLit   False) = data_ "False"
printExpr UnitLit           = data_ "()"
printExpr (FCall proc args) =
  "$fcall" <+> pretty proc <+> hsep (map printExpr' args)

-- print an expression with unambiguous precendence
printExpr' :: Syn -> Document
printExpr' e@App{}   = parens (printExpr e)
printExpr' e@Let{}   = parens (printExpr e)
printExpr' e@Case{}  = parens (printExpr e)
printExpr' e@MCase{} = parens (printExpr e)
printExpr' e@FCall{} = parens (printExpr e)
printExpr' e         = printExpr e

printInterpolatedString :: String -> [(Syn, String)] -> Document
printInterpolatedString prefix interps = dquotes str
 where
  str = pretty (escape prefix)
    <> hcat (map (\(e, s) -> printInterp e <> pretty (escape s)) interps)
  printInterp e = "#{" <> printExpr e <> "}"

escape :: String -> String
escape ('"'       : s) = '\\' : '"' : escape s
escape ('\\'      : s) = '\\' : '\\' : escape s
escape ('\n'      : s) = '\\' : 'n' : escape s
escape ('#' : '{' : s) = '#' : '\\' : '{' : escape s
escape (x         : s) = x : escape s
escape []              = []

-- Can we simplify this by introducting printExpr' which behaves like printExpr
-- but always parenthesises applications?
printApp :: Syn -> Syn -> Document
printApp (App (Var op) a) b | op `elem` binOps =
  parens $ case (singleton a, singleton b) of
    (True, True) -> printExpr a <+> printExpr (Var op) <+> printExpr b
    (False, False) ->
      parens (printExpr a) <+> printExpr (Var op) <+> parens (printExpr b)
    (True, False) ->
      printExpr a <+> printExpr (Var op) <+> parens (printExpr b)
    (False, True) ->
      parens (printExpr a) <+> printExpr (Var op) <+> printExpr b

-- special case for the only infix constructor: (::)
printApp (App (Con "::") a) b = parens $ case (singleton a, singleton b) of
  (True , True ) -> printExpr a <+> "::" <+> printExpr b
  (False, False) -> parens (printExpr a) <+> "::" <+> parens (printExpr b)
  (True , False) -> printExpr a <+> "::" <+> parens (printExpr b)
  (False, True ) -> parens (printExpr a) <+> "::" <+> printExpr b

printApp a (App b c) = if big a
  then parens (printExpr a) <+> nest 2 (parens (printExpr (App b c)))
  else printExpr a <+> parens (printExpr (App b c))
printApp a b | big a && big b =
  parens (printExpr a) <+> nest 2 (parens (printExpr b))
printApp a b | big a = parens (printExpr a) <+> nest 2 (printExpr b)
printApp a b | big b = printExpr a <+> parens (printExpr b)
printApp a b         = printExpr a <+> printExpr b

-- Used if the list is 'big'
printList :: [Syn] -> Document
printList es = hang 2 $ encloseSep lbracket rbracket comma (map printExpr es)

-- let x = 1
--     y = 2
--  in expr
--
-- The hang 1 pushes 'in' forwards to line up with the end of 'let'
printLet :: [(RawName, Syn, Maybe Type)] -> Syn -> Document
printLet binds e = hang 1 $ vsep
  [ keyword "let" <+> align (vsep (map printLetBind binds))
  , keyword "in" <+> printExpr e
  ]

 where
  printLetBind (name, expr, Nothing) =
    printName name <+> "=" <+> printExpr expr
  printLetBind (name, expr, Just ty) =
    (printName name <+> ":" <+> printType ty) <> hardline <> printLetBind
      (name, expr, Nothing)

-- case expr of
--   pat1 x y -> e1
--   pat2 z w -> e2
printCase :: Syn -> [(Syn.Pattern, Syn)] -> Document
printCase e alts =
  hang 2
    $ forceVSep
    $ (keyword "case" <+> printExpr e <+> keyword "of")
    : map printAlt alts
  where printAlt (pat, expr) = printPattern pat <+> "->" <+> printExpr expr

-- pat1 pat2 -> e1
-- pat3 pat4 -> e2
printMCase :: [([Syn.Pattern], Syn)] -> Document
printMCase alts = parens $ hang 0 $ forceVSep $ map printAlt alts
 where
  printAlt (pats, expr) =
    hsep (map printPattern pats) <+> "->" <+> printExpr expr

printData :: Data -> Document
printData d =
  keyword "type"
    <+> printName (dataName d)
    <+> hsep (map printName (dataTyVars d))
    <+> equals
    <+> hsep (punctuate pipe (map printCon (dataCons d)))

printAlias :: Alias -> Document
printAlias a =
  keyword "type alias"
    <+> printName (aliasName a)
    <+> hsep (map printName (aliasTyVars a))
    <+> equals
    <+> printType (aliasType a)

printCon :: DataCon -> Document
printCon DataCon { conName = name, conArgs = args } =
  printName name <+> hsep (map (printType' AppR) args)

printComment :: String -> Document
printComment c = "--" <+> pretty c

printName :: RawName -> Document
printName (Name n) = pretty n

printPackageName :: PackageName -> Document
printPackageName n = pretty (show n)

prettyModuleName :: ModuleName -> Document
prettyModuleName (ModuleName names) = hcat (map pretty (intersperse "." names))

-- True if we would never need to parenthesise it
singleton :: Syn -> Bool
singleton (Var      _      ) = True
singleton (Hole     _      ) = True
singleton (Con      _      ) = True
singleton (IntLit   _      ) = True
singleton (TupleLit _      ) = True
singleton (ListLit  _      ) = True
singleton (StringInterp _ _) = True
singleton (StringLit _     ) = True
singleton _                  = False

big :: Syn -> Bool
big (Case _ _  ) = True
big (Let  _ _  ) = True
big (App  a b  ) = size (App a b) > 5
big (ListLit xs) = any big xs
big _            = False

size :: Syn -> Int
size (App a b) = size a + size b
size _         = 1

printRecordSyntax :: Doc a -> [(Doc a, Doc a)] -> Doc a
printRecordSyntax separator rec =
  braces' $ hsep $ punctuate comma (map (\(n, t) -> n <+> separator <+> t) rec)

-- Like tupled but will always print everything on the same line
htupled :: [Doc a] -> Doc a
htupled elems = mconcat (zipWith (<>) (lparen : repeat comma) elems) <> rparen

-- Like braces but will pad with a space on either side
braces' :: Doc a -> Doc a
braces' d = "{" <+> d <+> "}"

-- Like vsep but uses hardline instead of line to prevent 'group' from removing the line breaks.
forceVSep :: [Doc a] -> Doc a
forceVSep = concatWith (\x y -> x <> hardline <> y)
