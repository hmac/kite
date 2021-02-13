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
import           Type                           ( CtorInfo )
import           Data.Name
import           Util

convertModule :: CtorInfo -> Can.Module -> T.Module
convertModule ctorInfo modul = T.Module
  { T.moduleName    = moduleName modul
  , T.moduleImports = moduleImports modul
  , T.moduleExports = map fst (moduleExports modul)
  , T.moduleDecls   = map (T.FunDecl . convertFun ctorInfo) (funDecls modul)
                        <> map (T.DataDecl . convertData) (dataDecls modul)
  }

convertFun :: CtorInfo -> Can.Fun Can.Exp -> T.Fun
convertFun ctorInfo fun = T.Fun
  { T.funName = funName fun
  , T.funType = unknown
  , T.funExpr = convertExpr ctorInfo (funExpr fun)
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

convertPattern :: CtorInfo -> Can.Pattern -> T.Pattern
convertPattern ctorInfo = \case
  ConsPat c _ args -> case lookup c ctorInfo of
    Just (tag, arity, typeName) -> T.ConsPat
      c
      (Just (ConMeta tag arity typeName))
      (map (convertPattern ctorInfo) args)
    Nothing ->
      error
        $  "Type.ToTyped.convertPattern: No metadata for constructor "
        <> show c
  TuplePat pats -> TuplePat $ map (convertPattern ctorInfo) pats
  ListPat  pats -> ListPat $ map (convertPattern ctorInfo) pats
  p             -> p

convertExpr :: CtorInfo -> Can.Exp -> T.Exp
convertExpr ctorInfo expr = go expr
 where
  go = \case
    Var v     -> T.VarT v unknown
    Ann _e _t -> error "Type.ToTyped.convertExpr: cannot convert annotations"
    Con c     -> case lookup c ctorInfo of
      Just (tag, arity, typeName) ->
        T.ConT c (ConMeta tag arity typeName) unknown
      Nothing ->
        error
          $  "Type.ToTyped.convertExpr: No metadata for constructor "
          <> show c
    Hole n      -> T.HoleT n unknown
    Abs xs    e -> T.AbsT (map (, unknown) xs) (go e) unknown
    App a     b -> T.AppT (go a) (go b) unknown
    Let binds e -> T.LetT
      (map (\(n, v, ty) -> (n, go v, convertType <$> ty)) binds)
      (go e)
      unknown
    Case s alts ->
      let convertAlt :: (Can.Pattern, Can.Exp) -> (T.Pattern, T.Exp)
          convertAlt (p, e) = (convertPattern ctorInfo p, go e)
      in  T.CaseT (go s) (map convertAlt alts) unknown
    MCase alts ->
      T.MCaseT (map (bimap (map (convertPattern ctorInfo)) go) alts) unknown
    UnitLit              -> T.UnitLitT
    TupleLit es          -> T.TupleLitT (map go es) unknown
    ListLit  es          -> T.ListLitT (map go es) unknown
    StringInterp s comps -> T.StringInterpT s (mapFst go comps)
    StringLit s          -> T.StringLitT s
    CharLit   c          -> T.CharLitT c
    IntLit    i          -> T.IntLitT i
    BoolLit   b          -> T.BoolLitT b
    Record    fields     -> T.RecordT (mapSnd go fields) unknown
    Project r f          -> T.ProjectT (go r) f unknown
    FCall   f args       -> T.FCallT f (map go args) unknown


unknown :: Type
unknown = UType (U 0 "unknown")
