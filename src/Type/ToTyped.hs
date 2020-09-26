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
import           Type                           ( Type(..)
                                                , U(..)
                                                )
import           AST
import           Syn                     hiding ( Name
                                                , Type
                                                )
import qualified Syn.Typed                     as T
import           Data.Coerce                    ( coerce )
import           Data.Name
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
                       , T.funType = unknown
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
                               , T.conType = unknown
                               }

convertType :: Can.Type -> Type
convertType _ = unknown

convertPattern :: Can.Pattern -> T.Pattern
convertPattern = coerce

convertExpr :: Can.Exp -> T.Exp
convertExpr = \case
  Var v     -> T.VarT v unknown
  Ann _e _t -> error "Type.ToTyped.convertExpr: cannot convert annotations"
  Con  c    -> T.ConT c
  Hole n    -> T.HoleT n unknown
  Abs xs e  -> T.AbsT (map (, unknown) xs) (convertExpr e) unknown
  App a  b  -> T.AppT (convertExpr a) (convertExpr b) unknown
  LetA n ty v e ->
    T.LetAT n (convertType ty) (convertExpr v) (convertExpr e) unknown
  Let  binds e    -> T.LetT (mapSnd convertExpr binds) (convertExpr e) unknown
  Case s     alts -> T.CaseT (convertExpr s) (map convertAlt alts) unknown
  MCase alts      -> T.MCaseT
    (map (\(pats, expr) -> (map convertPattern pats, convertExpr expr)) alts)
    unknown
  UnitLit              -> T.UnitLitT
  TupleLit es          -> T.TupleLitT (map convertExpr es) unknown
  ListLit  es          -> T.ListLitT (map convertExpr es) unknown
  StringInterp s comps -> T.StringInterpT s (mapFst convertExpr comps)
  StringLit s          -> T.StringLitT s
  CharLit   c          -> T.CharLitT c
  IntLit    i          -> T.IntLitT i
  BoolLit   b          -> T.BoolLitT b
  Record    fields     -> T.RecordT (mapSnd convertExpr fields) unknown
  Project r f          -> T.ProjectT (convertExpr r) f unknown
  FCall   f args       -> T.FCallT f (map convertExpr args) unknown

convertAlt :: (Can.Pattern, Can.Exp) -> (T.Pattern, T.Exp)
convertAlt (p, e) = (convertPattern p, convertExpr e)

unknown :: Type
unknown = UType (U 0 "unknown")
