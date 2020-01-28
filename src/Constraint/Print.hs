module Constraint.Print where

-- A printer for constraints

import           Constraint
import           Data.Text.Prettyprint.Doc
import           Data.Name
import           Canonical                      ( Name(..) )
import           Data.List                      ( intersperse )

printCConstraint :: CConstraint -> Doc a
printCConstraint (Simple c) = printConstraint c
printCConstraint (c :^^: d) = printCConstraint c <+> "^" <+> printCConstraint d
printCConstraint (E vars q c) =
  "∃"
    <+> parens (hsep (map printVar vars))
    <+> printConstraint q
    <+> "⊃"
    <+> printCConstraint c

printConstraint :: Constraint -> Doc a
printConstraint CNil      = "ϵ"
printConstraint (c :^: d) = printConstraint c <+> "^" <+> printConstraint d
printConstraint (t :~: v) = printType t <+> "~" <+> printType v

printVar :: Var -> Doc a
printVar (U n) = "U" <> printName n
printVar (R n) = "R" <> printName n

printType :: Type -> Doc a
printType (TCon t [a, b]) | t == TopLevel modPrim "->" =
  printType' a <+> "->" <+> printType' b
printType (TVar v     ) = printVar v
printType (TCon c args) = hsep (printName c : map printType' args)

-- like printType but assume we're in a nested context, so add parentheses
printType' :: Type -> Doc a
printType' (TCon t [a, b]) | t == TopLevel modPrim "->" =
  parens $ printType a <+> "->" <+> printType b
printType' (TVar v     ) = printVar v
printType' (TCon c args) = parens $ hsep (printName c : map printType args)

printName :: Name -> Doc a
printName (Local (Name n)) = pretty n
printName (TopLevel moduleName (Name n)) =
  printModuleName moduleName <> "." <> pretty n

printModuleName :: ModuleName -> Doc a
printModuleName (ModuleName names) = hcat (map pretty (intersperse "." names))
