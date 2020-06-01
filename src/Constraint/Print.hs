module Constraint.Print where

-- A printer for constraints

import           Constraint
import           Data.Text.Prettyprint.Doc     as PP
import           Data.Name
import           Data.List                      ( intersperse )
import qualified Data.Set                      as Set

printCConstraint :: CConstraint -> Doc a
printCConstraint (Simple cs) = PP.list (map printConstraint cs)
printCConstraint (E vars qs cs) =
  "∃"
    <+> parens (hsep (map printVar vars))
    <+> PP.list (map printConstraint qs)
    <+> "⊃"
    <+> PP.list (map printCConstraint cs)

printConstraint :: Constraint -> Doc a
printConstraint (t :~: v) = printType t <+> "~" <+> printType v
printConstraint (HasField r l t) =
  printType r <+> "~" <+> braces (printNameAsLocal l <+> colon <+> printType t)

printVar :: Var -> Doc a
printVar (U n) = "U" <> printName n
printVar (R n) = printName n

printType :: Type -> Doc a
printType (TApp (TApp (TCon t) a) b) | t == TopLevel modPrim "->" =
  printType' a <+> "->" <+> printType' b
printType (TApp a b)       = printType a <+> printType b
printType (TVar  v )       = printVar v
printType (TCon  n )       = printName n
printType (THole n )       = "?" <> printName n
printType TInt             = "Int"
printType TString          = "String"
printType TBool            = "Bool"
printType (TRecord fields) = printType' (TRecord fields)
printType (TAlias n _    ) = printName n

-- like printType but assume we're in a nested context, so add parentheses
printType' :: Type -> Doc a
printType' (TApp (TApp (TCon t) a) b) | t == TopLevel modPrim "->" =
  parens $ printType a <+> "->" <+> printType b
printType' (TApp a b)       = parens $ printType a <+> printType b
printType' (TVar  v )       = printVar v
printType' (TCon  n )       = printName n
printType' (THole n )       = "?" <> printName n
printType' TInt             = "Int"
printType' TString          = "String"
printType' TBool            = "Bool"
printType' (TRecord fields) = braces $ hsep $ punctuate
  comma
  (map (\(n, t) -> printName n <+> ":" <+> printType t) fields)
printType' (TAlias n _) = printName n

printName :: Name -> Doc a
printName (Local (Name n)) = pretty n
printName (TopLevel moduleName (Name n)) =
  printModuleName moduleName <> "." <> pretty n

printNameAsLocal :: Name -> Doc a
printNameAsLocal (TopLevel _ n) = printName (Local n)
printNameAsLocal n              = printName n

printModuleName :: ModuleName -> Doc a
printModuleName (ModuleName names) = hcat (map pretty (intersperse "." names))

printError :: Error -> Doc a
printError (OccursCheckFailure t v) =
  "Occurs check failure between" <+> printType t <+> "and" <+> printType v
printError (ConstructorMismatch t v) =
  "Constructors do not match between" <+> printType t <+> "and" <+> printType v
printError (UnsolvedConstraints cs) =
  "Unsolved constraints:" <+> PP.list (map printConstraint cs)
printError EquationsHaveDifferentNumberOfPatterns =
  "Equations have different number of patterns"
printError (UnsolvedUnificationVariables vars constraints) = vsep
  [ "Unsolved unification variables:"
  , hsep (map printVar (Set.toList vars))
  , "In constraints:"
  , PP.list (map printConstraint constraints)
  ]
printError (UnknownVariable v)       = "Unknown variable" <+> printName v
printError EmptyCase                 = "Empty case expression"
printError DuplicatePatternVariables = "Duplicate variables in pattern"
printError (RecordDoesNotHaveLabel ty name) =
  "The record type"
    <+> printType ty
    <+> "was expected to contain the label"
    <+> printName name
    <+> "but it does not."
printError (ProjectionOfNonRecordType ty name) =
  "I cannot get the field"
    <+> printName name
    <+> "from the type"
    <+> printType ty
    <+> "because it is not a record type."

printLocatedError :: LocatedError -> Doc a
printLocatedError (LocatedError moduleName err) = vsep
  [ "In the definition"
  <+> printName moduleName
  <+> "I encountered the following error:"
  , printError err
  ]
