module Type.ToTyped
  ( convertModule
  ) where

-- Convert Can.Module to T.Module
-- Right now we do this just so we can plug the output of the typechecking
-- easily into the rest of the compiler (which expects T.Module). As a result
-- there aren't any actual type annotations in this output and it's no more
-- informative than the untyped version. In future the typechecker should
-- actually construct T.Exp as it goes, inserting the correct type annotations.

import           AST
import qualified Canonical                     as Can
import qualified Data.Map.Strict               as Map
import           Data.Name
import           Syn                     hiding ( Name
                                                , Type
                                                )
import qualified Syn.Typed                     as T
import           Type.Primitive                 ( primitiveCtorInfo )
import           Type.Type                      ( CtorInfo
                                                , Type(..)
                                                , U(..)
                                                )
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
  { T.funName   = funName fun
  , T.funType   = unknown
  , T.funExpr   = convertExpr ctorInfo (funExpr fun)
  , T.funWheres = map (convertFun ctorInfo) (funWheres fun)
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
  ConsPat c _ args -> case Map.lookup c (ctorInfo <> primitiveCtorInfo) of
    Just meta -> T.ConsPat c (Just meta) (map (convertPattern ctorInfo) args)
    Nothing ->
      error
        $  "Type.ToTyped.convertPattern: No metadata for constructor "
        <> show c
  TuplePat pats -> TuplePat $ map (convertPattern ctorInfo) pats
  ListPat  pats -> ListPat $ map (convertPattern ctorInfo) pats
  p             -> p

convertExpr :: CtorInfo -> Can.Exp -> T.Exp
convertExpr ctorInfo = go
 where
  go = \case
    Var v     -> T.VarT unknown v
    Ann _e _t -> error "Type.ToTyped.convertExpr: cannot convert annotations"
    Con c     -> case Map.lookup c (ctorInfo <> primitiveCtorInfo) of
      Just meta -> T.ConT unknown c meta
      Nothing ->
        error
          $  "Type.ToTyped.convertExpr: No metadata for constructor "
          <> show c
    Hole n      -> T.HoleT unknown n
    Abs xs    e -> T.AbsT unknown (map (, unknown) xs) (go e)
    App a     b -> T.AppT unknown (go a) (go b)
    Let binds e -> T.LetT
      unknown
      (map (\(n, v, ty) -> (n, go v, convertType <$> ty)) binds)
      (go e)
    Case s alts ->
      let convertAlt :: (Can.Pattern, Can.Exp) -> (T.Pattern, T.Exp)
          convertAlt (p, e) = (convertPattern ctorInfo p, go e)
      in  T.CaseT unknown (go s) (map convertAlt alts)
    MCase alts ->
      T.MCaseT unknown (map (bimap (map (convertPattern ctorInfo)) go) alts)
    UnitLit              -> T.UnitLitT unknown
    TupleLit es          -> T.TupleLitT unknown (map go es)
    ListLit  es          -> T.ListLitT unknown (map go es)
    StringInterp s comps -> T.StringInterpT unknown s (fmap (first go) comps)
    StringLit s          -> T.StringLitT unknown s
    CharLit   c          -> T.CharLitT unknown c
    IntLit    i          -> T.IntLitT unknown i
    BoolLit   b          -> T.BoolLitT unknown b
    Record    fields     -> T.RecordT unknown (mapSnd go fields)
    Project r f          -> T.ProjectT unknown (go r) f
    FCall   f args       -> T.FCallT unknown f (map go args)


unknown :: Type
unknown = UType (U 0 "unknown")
