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
  S.UnitLit             -> E.UnitLit
  S.Record fields       -> E.Record $ mapSnd fromSyn fields
  S.Project r    l      -> E.Project (fromSyn r) l
  S.FCall   proc args   -> E.FCall proc (map fromSyn args)

-- | Extract all free ty vars in Forall, then convert Syn.Ty to Constraint.Type
tyToScheme :: S.Type_ Name -> E.Scheme
tyToScheme t =
  let t'   = tyToType t
      vars = Set.toList (ftv t')
  in  E.Forall vars t'

schemeToScheme :: Can.Scheme -> E.Scheme
schemeToScheme (S.Forall vs t) =
  let t'   = tyToType t
      vars = Set.toList (ftv t') <> map R vs
  in  E.Forall vars t'

tyToType :: S.Type_ Name -> Type
tyToType = \case
  S.TyCon n         -> (TCon n)
  S.TyAlias n a     -> TAlias n (tyToType a)
  S.TyApp   a b     -> TApp (tyToType a) (tyToType b)
  S.TyVar n         -> TVar (R n)
  S.TyList          -> TCon (TopLevel modPrim "List")
  S.TyTuple ts      -> mkTupleType (map tyToType ts)
  S.TyHole  n       -> THole (Local n)
  S.TyInt           -> TInt
  S.TyString        -> TString
  S.TyChar          -> TChar
  S.TyBool          -> TBool
  S.TyUnit          -> TUnit
  S.TyFun a b       -> tyToType a `fn` tyToType b
  S.TyRecord fields -> TRecord $ mapSnd tyToType fields
