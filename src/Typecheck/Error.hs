module Typecheck.Error where

import           Typecheck.THIH

-- In future we'll render source locations here
printError :: Error -> String
printError = \case
  UnboundIdentifier i -> "Unbound identifier: " <> i
  CannotUnify t1 t2 -> "Cannot unify " <> printType t1 <> " with " <> printType t2
  OccursCheckFailed (Tyvar name k) ty ->
    "Occurs check failed on " <> name <> " of kind " <> printKind k <> " and " <> printType ty
  TypesDontMatch t1 t2 -> "Types don't match: " <> printType t1 <> " and " <> printType t2
  KindsDontMatch (Tyvar name k) t ->
    "The kind of " <> show name <> " is " <> printKind k
      <> ", which doesn't match the kind of " <> printType t
  -- TODO: the rest
  err -> show err

-- TODO: use prettyprinter
printType :: Type -> String
printType = \case
  TVar (Tyvar name _) -> name
  TCon (Tycon name _) -> name
  TAp t1 t2 -> printType t1 <> " " <> printType t2
  TGen i -> show i

printKind :: Kind -> String
printKind = \case
  Star -> "*"
  Kfun k1 k2 -> "(" <> printKind k1 <> " -> " <> printKind k2 <> ")"
