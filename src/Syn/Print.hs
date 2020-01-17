module Syn.Print where

import           Data.Maybe                     ( catMaybes )
import           Data.List                      ( intersperse )
import           Prelude                 hiding ( mod )

import           Data.Text.Prettyprint.Doc

import           Syntax

-- Semantic annotation
data Style = VarStyle | KeywordStyle | FunctionStyle | TypeStyle | DataStyle | HoleStyle

var :: Document -> Document
var = annotate VarStyle

keyword :: Document -> Document
keyword = annotate KeywordStyle

func :: Document -> Document
func = annotate FunctionStyle

type_ :: Document -> Document
type_ = annotate TypeStyle

data_ :: Document -> Document
data_ = annotate DataStyle

hole :: Document -> Document
hole = annotate HoleStyle

type Document = Doc Style

-- TODO: we need to parse and then print comments, too
printModule :: Module Syn -> Document
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
printMetadata :: [(String, String)] -> Maybe Document
printMetadata []  = Nothing
printMetadata kvs = Just $ vsep ["---", printKvs, "---"]
  where printKvs = vsep $ map (\(k, v) -> hcat [pretty k, ": ", pretty v]) kvs

-- module Foo
printModName :: ModuleName -> Document
printModName (ModuleName names) =
  keyword "module" <+> hcat (map pretty (intersperse "." names))

--   (fun1, fun2)
printModExports :: [Name] -> Maybe Document
printModExports []      = Nothing
printModExports exports = Just $ tupled (map printName exports)

-- import Data.Text
-- import qualified Data.Text.Encoding as E (encodeUtf8)
printImports :: [Import] -> Maybe Document
printImports []      = Nothing
printImports imports = Just $ vsep (map printImport imports)

printImport :: Import -> Document
printImport i = hsep
  [ keyword "import"
  , if importQualified i then keyword "qualified" else "         "
  , prettyModuleName (importName i)
  , maybe mempty (\n -> keyword "as" <+> printName n) (importAlias i)
  , tupled (map printName (importItems i))
  ]

printModDecls :: [Decl Syn] -> Maybe Document
printModDecls []    = Nothing
printModDecls decls = Just $ vsep (intersperse mempty (map printDecl decls))

printDecl :: Decl Syn -> Document
printDecl (Comment       c) = printComment c
printDecl (FunDecl       f) = printFun f
printDecl (DataDecl      d) = printData d
printDecl (TypeclassDecl t) = printTypeclass t
printDecl (TypeclassInst i) = printInstance i

printFun :: Fun Syn -> Document
printFun Fun { funComments = comments, funName = name, funDefs = defs, funType = ty, funConstraint = constraint }
  = vsep $ printComments comments ++ [sig] ++ map (printDef name) defs
 where
  sig = case constraint of
    Just c -> printName name <> align
      (space <> colon <+> printConstraint c <+> "=>" <+> printType ty)
    Nothing -> printName name <> align (space <> colon <+> printType ty)
  printComments [] = []
  printComments cs = map printComment cs

-- TODO: properly print 3+ element constraint tuples
printConstraint :: Constraint -> Document
printConstraint (CInst tyclass vars) =
  printName tyclass <+> hsep (map printType vars)
printConstraint (CTuple a b) = tupled [printConstraint a, printConstraint b]
-- a -> b
-- f a -> f b
-- a -> b -> c
-- (a -> b) -> c
printType :: Ty -> Document
printType = printType' Root

data Context = Root | AppL | AppR  | ArrL | ArrR

printType' :: Context -> Ty -> Document
printType' ctx ty = case (ctx, ty) of
  -- top level arrows don't get parenthesised
  (Root, (TyArr :@: a) :@: b) ->
    printType' ArrL a <+> "->" <+> printType' ArrR b
  -- top level applications don't get parenthesised either
  (Root, a :@: b            ) -> printType' AppL a <+> printType' AppR b

  -- arrows on the left of arrows get parenthesised
  (ArrL, (TyArr :@: a) :@: b) -> parens $ printType' Root (a `fn` b)
  -- arrows on the right of arrows don't
  (ArrR, (TyArr :@: a) :@: b) -> printType' Root (a `fn` b)
  -- arrows on either side of applications get parenthesised
  (AppR, (TyArr :@: a) :@: b) -> parens $ printType' Root (a `fn` b)
  (AppL, (TyArr :@: a) :@: b) -> parens $ printType' Root (a `fn` b)

  -- applications on the left of applications don't get parenthesised
  (AppL, a :@: b            ) -> printType' Root (a :@: b)
  -- applications on the right of applications get parenthesised
  (AppR, a :@: b            ) -> parens $ printType' Root (a :@: b)
  -- applications on either side of arrows don't
  (ArrL, a :@: b            ) -> printType' Root (a :@: b)
  (ArrR, a :@: b            ) -> printType' Root (a :@: b)

  -- Basic cases
  (_   , TyHole n           ) -> hole ("?" <> printName n)
  (_   , TyCon n            ) -> printName n
  (_   , TyVar n            ) -> printName n
  (_   , TyList ts          ) -> brackets (printType' Root ts)
  (_   , TyTuple ts         ) -> tupled (map (printType' Root) ts)
  (_   , TyInt              ) -> "Int"
  (_   , TyString           ) -> "String"
  (_   , _                  ) -> "nope"

-- For "big" expressions, print them on a new line under the =
-- For small expressions, print them on the same line
printDef :: Name -> Def Syn -> Document
printDef name d | big (defExpr d) = nest 2 $ vsep [lhs, printExpr (defExpr d)]
                | otherwise       = lhs <+> printExpr (defExpr d)
  where lhs = printName name <+> hsep (map printPattern (defArgs d)) <+> equals

printPattern :: Pattern -> Document
printPattern (VarPat n)      = printName n
printPattern WildPat         = "_"
printPattern (IntPat   i   ) = pretty i
printPattern (TuplePat pats) = tupled (map printPattern pats)
printPattern (ListPat  pats) = list (map printPattern pats)
-- special case for the only infix constructor: (::)
printPattern (ConsPat "::" [x, y]) =
  parens $ printPattern x <+> "::" <+> printPattern y
printPattern (ConsPat n pats) =
  parens $ printName n <+> hsep (map printPattern pats)

-- TODO: binary operators
printExpr :: Syn -> Document
printExpr (Var  n) = printName n
printExpr (Cons n) = data_ (printName n)
printExpr (Hole n) = hole ("?" <> printName n)
printExpr (Abs args e) =
  parens $ "\\" <> hsep (map printName args) <+> "->" <+> printExpr e
printExpr (App  a     b   ) = printApp a b
printExpr (Let  binds e   ) = printLet binds e
printExpr (Case e     alts) = printCase e alts
printExpr (TupleLit es    ) = tupled (map printExpr es)
printExpr (ListLit es) | any big es = printList es
                       | otherwise  = list (map printExpr es)
printExpr (IntLit i                ) = pretty i
printExpr (StringLit prefix interps) = printInterpolatedString prefix interps

printInterpolatedString :: String -> [(Syn, String)] -> Document
printInterpolatedString prefix interps = dquotes str
 where
  str = pretty (escape prefix)
    <> hcat (map (\(e, s) -> printInterp e <> pretty (escape s)) interps)
  printInterp e = "#{" <> printExpr e <> "}"

escape :: String -> String
escape ('"' : s) = '\\' : '"' : escape s
escape (x   : s) = x : escape s
escape []        = []

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
printApp (App (Cons "::") a) b = parens $ case (singleton a, singleton b) of
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
printList es = nest
  2
  (encloseSep (lbracket <> line) (line <> rbracket) comma (map printExpr es))

-- let x = 1
--     y = 2
--  in expr
--
--  The hang (-3) pushes 'in' back to end in line with 'let'
printLet :: [(Name, Syn)] -> Syn -> Document
printLet binds e = keyword "let" <+> hang
  (-3)
  (vsep [hang 0 (vsep (map printLetBind binds)), keyword "in" <+> printExpr e])
  where printLetBind (name, expr) = printName name <+> "=" <+> printExpr expr

-- case expr of
--   pat1 x y -> e1
--   pat2 z w -> e2
printCase :: Syn -> [(Pattern, Syn)] -> Document
printCase e alts = keyword "case"
  <+> hang (-3) (vsep ((printExpr e <+> keyword "of") : map printAlt alts))
  where printAlt (pat, expr) = printPattern pat <+> "->" <+> printExpr expr

printData :: Data -> Document
printData d =
  keyword "data"
    <+> printName (dataName d)
    <+> hsep (map printName (dataTyVars d))
    <+> equals
    <+> hsep (punctuate pipe (map printCon (dataCons d)))

printCon :: DataCon -> Document
printCon DataCon { conName = name, conArgs = args } =
  printName name <+> hsep (map printType args)
printCon RecordCon { conName = name, conFields = fields } =
  printName name <+> braces
    (hsep
      (punctuate comma
                 (map (\(n, t) -> printName n <+> ":" <+> printType t) fields)
      )
    )

printTypeclass :: Typeclass -> Document
printTypeclass t = vsep (header : map printTypeclassDef (typeclassDefs t))
 where
  header =
    keyword "class"
      <+> printName (typeclassName t)
      <+> hsep (map printName (typeclassTyVars t))
      <+> "where"
  printTypeclassDef (name, ty) =
    indent 2 $ printName name <+> colon <+> printType ty

printInstance :: Instance Syn -> Document
printInstance i = vsep (header : map printInstanceDef (instanceDefs i))
 where
  header =
    keyword "instance"
      <+> printName (instanceName i)
      <+> hsep (map printType (instanceTypes i))
      <+> "where"
  printInstanceDef (name, defs) = indent 2 $ vsep (map (printDef name) defs)

printComment :: String -> Document
printComment c = "--" <+> pretty c

printName :: Name -> Document
printName (Name n) = pretty n

prettyModuleName :: ModuleName -> Document
prettyModuleName (ModuleName names) = hcat (map pretty (intersperse "." names))

-- True if we would never need to parenthesise it
singleton :: Syn -> Bool
singleton (Var      _) = True
singleton (Hole     _) = True
singleton (Cons     _) = True
singleton (IntLit   _) = True
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
