module Constraint.FromSyn
  ( fromSyn
  , tyToType
  , tyToScheme
  )
where

-- Converts a Syn AST to a Constraint.Expr AST

import qualified Canonical                     as Can
import           Data.Name                      ( Name(..) )
import qualified Syn                           as S
import qualified Constraint.Expr               as E
import           Constraint
import           Util
import qualified Data.Set                      as Set

fromSyn :: Can.Exp -> E.Exp
fromSyn = \case
  S.Var  n      -> E.Var n
  S.Con  n      -> E.Con n
  S.Hole n      -> E.Hole n
  S.Abs xs    e -> E.Abs xs (fromSyn e)
  S.App a     b -> E.App (fromSyn a) (fromSyn b)
  S.Let binds e -> E.Let (mapSnd fromSyn binds) (fromSyn e)
  S.LetA x sch e body ->
    E.LetA x (schemeToScheme sch) (fromSyn e) (fromSyn body)
  S.Case scrut alts     -> E.Case (fromSyn scrut) (mapSnd fromSyn alts)
  S.TupleLit es         -> E.TupleLit (map fromSyn es)
  S.ListLit  es         -> E.ListLit (map fromSyn es)
  S.StringLit pre comps -> E.StringLit pre (mapFst fromSyn comps)
  S.IntLit  i           -> E.IntLit i
  S.BoolLit b           -> E.BoolLit b
  S.Record  fields      -> E.Record $ mapSnd fromSyn fields
  S.Project r l         -> E.Project (fromSyn r) l

-- | Extract all free ty vars in Forall, then convert Syn.Ty to Constraint.Type
tyToScheme :: Maybe (S.Constraint_ Name) -> S.Type_ Name -> E.Scheme
tyToScheme c t =
  let t'         = tyToType t
      vars       = Set.toList (ftv t')
      constraint = maybe mempty constraintToConstraint c
  in  E.Forall vars constraint t'

schemeToScheme :: Can.Scheme -> E.Scheme
schemeToScheme (S.Forall vs c t) =
  let t'         = tyToType t
      vars       = Set.toList (ftv t') <> map R vs
      constraint = constraintToConstraint c
  in  E.Forall vars constraint t'

constraintToConstraint :: S.Constraint_ Name -> Constraint
constraintToConstraint S.CNil = CNil
constraintToConstraint (S.CTuple a b) =
  constraintToConstraint a :^: constraintToConstraint b

tyToType :: S.Type_ Name -> Type
tyToType = \case
  S.TyCon n         -> (TCon n)
  S.TyApp a b       -> TApp (tyToType a) (tyToType b)
  S.TyVar  n        -> TVar (R n)
  S.TyList t        -> list (tyToType t)
  S.TyBareList      -> TCon (TopLevel modPrim "List")
  S.TyTuple ts      -> mkTupleType (map tyToType ts)
  S.TyHole  n       -> THole (Local n)
  S.TyInt           -> TInt
  S.TyString        -> TString
  S.TyBool          -> TBool
  S.TyFun a b       -> tyToType a `fn` tyToType b
  S.TyRecord fields -> TRecord $ mapSnd tyToType fields
