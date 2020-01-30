module Constraint.FromSyn (fromSyn, tyToType, tyToScheme) where

-- Converts a Syn AST to a Constraint.Expr AST

import Canonical (Name(..))
import qualified Syntax as S
import qualified Constraint.Expr as E
import Constraint
import Util
import qualified Data.Set as Set

fromSyn :: S.Syn_ Name -> E.Exp
fromSyn = \case
  S.Var n -> E.Var n
  S.Cons n -> E.Con n
  S.Hole n -> E.Hole n
  S.Abs xs e -> E.Abs xs (fromSyn e)
  S.App a b -> E.App (fromSyn a) (fromSyn b)
  S.Let binds e -> E.Let (mapSnd fromSyn binds) (fromSyn e)
  S.LetA x t e body -> E.LetA x (tyToScheme t) (fromSyn e) (fromSyn body)
  S.Case scrut alts -> E.Case (fromSyn scrut) (map (\(p, e) -> E.Alt p (fromSyn e)) alts)
  S.TupleLit es -> E.TupleLit (map fromSyn es)
  S.ListLit es -> E.ListLit (map fromSyn es)
  S.StringLit pre comps -> E.StringLit pre (mapFst fromSyn comps)
  S.IntLit i -> E.IntLit i

-- Extract all free ty vars in Forall, then convert Syn.Ty to Constraint.Type
tyToScheme :: S.Ty_ Name -> E.Scheme
tyToScheme t = let t' = tyToType t
                   vars = Set.toList (ftv t')
                in E.Forall vars mempty t'

tyToType :: S.Ty_ Name -> Type
tyToType = \case
  S.TyArr -> TCon (TopLevel modPrim "->") []
  S.TyCon n -> TCon n []
  S.TyVar n -> TVar (R n)
  S.TyList t -> list (tyToType t)
  S.TyTuple ts -> mkTupleType (map tyToType ts)
  S.TyHole n -> THole (Local n)
  S.TyInt -> TInt
  S.TyString -> TString
  t S.:@: v -> let (conName, args) = flatten t
                in TCon conName (map tyToType (snoc args v))

flatten :: S.Ty_ Name -> (Name, [S.Ty_ Name])
flatten = \case
  t S.:@: v -> let (conName, args) = flatten t
                in (conName, snoc args v)
  S.TyCon n -> (n, [])
  S.TyArr   -> (TopLevel modPrim "->", [])
  t -> error $ "Unexpected type " <> show t
