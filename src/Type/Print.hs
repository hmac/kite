module Type.Print where

import           Data.Text.Prettyprint.Doc
import           Type
import           Data.Name
import           Data.List                      ( intersperse )

printModuleName :: ModuleName -> Doc a
printModuleName (ModuleName names) = hcat (map pretty (intersperse "." names))

printName :: Name -> Doc a
printName (Local (Name n)) = pretty n
printName (TopLevel moduleName (Name n)) =
  printModuleName moduleName <> "." <> pretty n

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
                | otherwise  -> go P0 f <+> hsep (map (go P2) args)
    Fn a b | prec >= P1 -> parens $ go P0 (Fn a b)
           | otherwise  -> go P1 a <+> "->" <+> go P0 b
    TCon "Kite.Primitive.List" [arg] -> brackets $ go P0 arg
    TCon con args | prec >= P2 -> parens $ go P0 (TCon con args)
                  | otherwise  -> printName con <+> hsep (map (go P0) args)
    EType   e      -> printE e
    UType   u      -> printU u
    TRecord fields -> braces $ sep $ punctuate comma $ map
      (\(f, ty) -> (pretty f) <+> colon <+> (printType ty))
      fields
    Forall v ty -> printForall [v] ty
     where
      printForall us (Forall u a) = printForall (u : us) a
      printForall us a =
        "forall" <+> hsep (map printU (reverse us)) <> "." <+> printType a

printU :: U -> Doc a
printU (U n v) = printName v <> pretty n

printE :: E -> Doc a
printE (E n) = pretty $ "ê" <> show n

printError :: Maybe ModuleName -> Error -> Doc a
printError modName = \case
  SubtypingFailure a b ->
    vsep
      [ "Could not unify"
      , indent 4 (printType a)
      , "with"
      , indent 4 (printType b)
      ]
  UnknownVariable (Free v) ->
    "Variable not in scope:" <+> printName (maybe v (`localise` v) modName)
  DuplicateVariable (Free v) ->
    "The variable"
      <+> printName (maybe v (`localise` v) modName)
      <+> "is bound more than once"
  e -> pretty (show e)
