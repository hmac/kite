module Type.ToTyped
  ( convertModule
  )
where

-- Convert Can.Module to T.Module
-- Right now we do this just so we can plug the output of the typechecking
-- easily into the rest of the compiler (which expects T.Module). As a result
-- there aren't any actual type annotations in this output and it's no more
-- informative than the untyped version. In future the typechecker should
-- actually construct T.Exp as it goes, inserting the correct type annotations.

import qualified Canonical                     as Can
import           Syn                     hiding ( Name )
import qualified Syn.Typed                     as T
import           Data.Coerce                    ( coerce )
import           Data.Name
import qualified Constraint                    as C
                                                ( Var(R)
                                                , fn
                                                )
import           Data.String                    ( fromString )
import           Util

convertModule :: Can.Module -> T.Module
convertModule modul = T.Module
  { T.moduleName    = moduleName modul
  , T.moduleImports = moduleImports modul
  , T.moduleExports = map fst (moduleExports modul)
  , T.moduleDecls   = map (T.FunDecl . convertFun) (funDecls modul)
                        <> map (T.DataDecl . convertData) (dataDecls modul)
  }

convertFun :: Can.Fun Can.Exp -> T.Fun
convertFun fun = T.Fun { T.funName = funName fun
                       , T.funType = convertMaybeType (funType fun)
                       , T.funExpr = convertExpr (funExpr fun)
                       }

convertData :: Can.Data -> T.Data
convertData d = T.Data { T.dataName   = dataName d
                       , T.dataTyVars = map Local (dataTyVars d)
                       , T.dataCons   = map convertDataCon (dataCons d)
                       }

convertDataCon :: Can.DataCon -> T.DataCon
convertDataCon con = T.DataCon { T.conName = conName con
                               , T.conArgs = map convertType (conArgs con)
                               , T.conType = T.Forall [] unknown
                               }

-- If nothing, return a hole
convertMaybeType :: Maybe Can.Type -> T.Scheme
convertMaybeType Nothing   = T.Forall [] unknown
convertMaybeType (Just ty) = go [] ty
 where
  go :: [Name] -> Can.Type -> T.Scheme
  go vars (TyForall v t) = go (v : vars) t
  go vars t              = T.Forall (map C.R (reverse vars)) (convertType t)

convertType :: Can.Type -> T.Type
convertType = \case
  TyCon c   -> T.TCon c
  TyApp a b -> T.TApp (convertType a) (convertType b)
  TyVar v   -> T.TVar (C.R v)
  TyList    -> T.TCon "Lam.Primitive.List"
  TyTuple ts ->
    let con = T.TCon $ fromString $ "Lam.Primitive.Tuple" <> show (length ts)
    in  foldl T.TApp con (map convertType ts)
  TyInt           -> T.TInt
  TyChar          -> T.TChar
  TyString        -> T.TString
  TyBool          -> T.TBool
  TyUnit          -> T.TUnit
  TyHole n        -> T.THole (Local n)
  TyFun a b       -> convertType a `C.fn` convertType b
  TyRecord fields -> T.TRecord (mapSnd convertType fields)
  TyAlias  a t    -> T.TAlias a (convertType t)
  TyForall _ _    -> error "Type.ToTyped: Cannot convert TyForall to T.Type"

convertScheme :: Can.Scheme -> T.Scheme
convertScheme (Forall vars ty) = T.Forall (map C.R vars) (convertType ty)

convertPattern :: Can.Pattern -> T.Pattern
convertPattern = coerce

convertExpr :: Can.Exp -> T.Exp
convertExpr = \case
  Var v     -> T.VarT v unknown
  Ann _e _t -> error "Type.ToTyped.convertExpr: cannot convert annotations"
  Con  c    -> T.ConT c
  Hole n    -> T.HoleT n unknown
  Abs x e   -> T.AbsT (map (, unknown) x) (convertExpr e)
  App a b   -> T.AppT (convertExpr a) (convertExpr b)
  LetA n sch v e ->
    T.LetAT n (convertScheme sch) (convertExpr v) (convertExpr e) unknown
  Let  binds e    -> T.LetT (mapSnd convertExpr binds) (convertExpr e) unknown
  Case s     alts -> T.CaseT (convertExpr s) (map convertAlt alts) unknown
  MCase alts      -> T.MCaseT
    (map (\(pats, expr) -> (map convertPattern pats, convertExpr expr)) alts)
    unknown
  UnitLit           -> T.UnitLitT unknown
  TupleLit es       -> T.TupleLitT (map convertExpr es) unknown
  ListLit  es       -> T.ListLitT (map convertExpr es) unknown
  StringLit s comps -> T.StringLitT s (mapFst convertExpr comps) unknown
  CharLit c         -> T.CharLitT c unknown
  IntLit  i         -> T.IntLitT i unknown
  BoolLit b         -> T.BoolLitT b unknown
  Record  fields    -> T.RecordT (mapSnd convertExpr fields) unknown
  Project r f       -> T.ProjectT (convertExpr r) f unknown
  FCall   f args    -> T.FCallT f (map convertExpr args) unknown

convertAlt :: (Can.Pattern, Can.Exp) -> T.AltT
convertAlt (p, e) = T.AltT (convertPattern p) (convertExpr e)

unknown :: T.Type
unknown = T.THole "unknown"
