module Type.Print where

import           Data.List                      ( intersperse )
import           Data.Name
import           Data.Text.Prettyprint.Doc
import qualified Prim
import           Type                           ( Error(..)
                                                , LocatedError(..)
                                                )
import           Type.Type                      ( E(..)
                                                , Type(..)
                                                , Type'(..)
                                                , U(..)
                                                )

printModuleName :: ModuleName -> Doc a
printModuleName (ModuleName names) = hcat (map pretty (intersperse "." names))

printPkgModuleName :: PkgModuleName -> Doc a
printPkgModuleName (PkgModuleName pkgName modName) =
  hcat [printPackageName pkgName, ".", printModuleName modName]

printPackageName :: PackageName -> Doc a
printPackageName (PackageName n) = pretty n

printName :: Name -> Doc a
printName (Local (Name n)) = pretty n
printName (TopLevel pkgModName (Name n)) =
  printPkgModuleName pkgModName <> "." <> pretty n

printLocatedError :: LocatedError -> Doc a
printLocatedError (LocatedError Nothing err) = vsep
  [ hsep
    ["In the definition", "<unknown>", "I encountered the following error:"]
  , ""
  , printError Nothing err
  ]
printLocatedError (LocatedError (Just (TopLevel modName fnName)) err) = vsep
  [ hsep
    [ "In the definition"
    , pretty (show fnName)
    , "I encountered the following error:"
    ]
  , ""
  , printError (Just modName) err
  ]
printLocatedError (LocatedError (Just (Local fnName)) err) = vsep
  [ hsep
    [ "In the definition"
    , printName (Local fnName)
    , "I encountered the following error:"
    ]
  , ""
  , printError Nothing err
  ]

-- Type operator precedence:
-- 0: default
-- 1: function arrows
-- 2. application
data Prec = P0 | P1 | P2 deriving (Eq, Ord)

-- Type operator associativity:
-- left: application
-- right: function arrows

printType :: Type -> Doc a
printType = go P0
 where
  go :: Prec -> Type -> Doc a
  go prec = \case
    TApp f args | prec >= P2 -> parens $ go P1 (TApp f args)
                | otherwise  -> go' P0 f <+> hsep (map (go P2) args)
    TCon c [arg] | c == prim "List" -> brackets $ go P0 arg
    TCon c args | c == prim "Tuple2" ->
      parens $ concatWith (surround ", ") $ map (go P0) args
    TCon con [] -> printConName con
    TCon con args | prec >= P2 -> parens $ go P0 (TCon con args)
                  | otherwise  -> printConName con <+> hsep (map (go P0) args)
    TOther t -> go' prec t
  go' prec = \case
    Fn a b | prec >= P1 -> parens $ go' P0 (Fn a b)
           | otherwise  -> go P1 a <+> "->" <+> go P0 b
    EType   e      -> printE e
    UType   u      -> printU u
    TRecord fields -> braces $ sep $ punctuate comma $ map
      (\(f, ty) -> pretty f <+> colon <+> printType ty)
      fields
    Forall v ty -> printForall [v] ty
     where
      printForall us (TOther (Forall u a)) = printForall (u : us) a
      printForall us a =
        "forall" <+> hsep (map printU (reverse us)) <> "." <+> printType a

-- If the constructor is in the Kite.Prim module, omit the module qualifier
printConName :: Name -> Doc a
printConName (TopLevel moduleName (Name n)) | moduleName == Prim.name = pretty n
printConName n = printName n

printU :: U -> Doc a
printU (U n v) = printName v <> pretty n

printE :: E -> Doc a
printE (E n) = pretty $ "ê" <> show n

-- TODO: complete this
printError :: Maybe PkgModuleName -> Error -> Doc a
printError modName = \case
  SubtypingFailure a b ->
    vsep
      [ "Could not unify"
      , indent 4 (printType a)
      , "with"
      , indent 4 (printType b)
      ]
  UnknownVariable v ->
    "Variable not in scope:" <+> printName (maybe v (`localise` v) modName)
  UnknownType t ->
    "Type not in scope:" <+> printName (maybe t (`localise` t) modName)
  DuplicateVariable v ->
    "The variable"
      <+> printName (maybe v (`localise` v) modName)
      <+> "is bound more than once"
  e -> pretty (show e)
