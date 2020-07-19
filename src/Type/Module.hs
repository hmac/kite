module Type.Module where

-- Typecheck a Lam module
-- Currently we just return unit (and an updated type env) if the module
-- typechecked. In future we will need to return a copy of the module with full
-- type annotations.

import qualified Data.Map.Strict               as Map

import qualified Syn                           as S
import           Syn                            ( Decl_(..)
                                                , Data_(..)
                                                , Module_(..)
                                                , DataCon_(..)
                                                , Fun_(..)
                                                )
import           Data.Name
import qualified Canonical                     as Can
import           Type                           ( TypeM
                                                , Type(..)
                                                , Ctx
                                                , CtxElem(Var)
                                                , V(..)
                                                )
import           Type.FromSyn                   ( convertScheme )

type TypeEnv = Map.Map Name Type

checkModule :: TypeEnv -> Can.Module -> TypeM (TypeEnv, ())
checkModule _env modul = do
  -- Extract type signatures from all datatype definitions in the module
  _dataTypeCtx <- mapM translateData (getDataDecls (moduleDecls modul))
  -- TODO: generate E vars for each function so they can be typechecked in any
  -- order
  -- Typecheck each function definition
  -- Done?
  undefined

-- Convert data type definitions into a series of <constructor, type> bindings.
--
--   data Maybe a = Just a | Nothing
-- becomes
--   Var (Free "Just") (Forall a. a -> Maybe a)
--   Var (Free "Nothing") (Forall a. Maybe a)
translateData :: Can.Data -> TypeM Ctx
translateData d =
  let tyvars = map Local (dataTyVars d)
  in  mapM (go (dataName d) tyvars) (dataCons d)
 where
  go :: Name -> [Name] -> Can.DataCon -> TypeM CtxElem
  go dataTypeName tyvars datacon = do
    -- Construct a (Syn) Scheme for this constructor
    let resultType =
          foldl (S.TyApp) (S.TyCon dataTypeName) (map S.TyVar tyvars)
    let scheme = S.Forall tyvars $ foldr S.TyFun resultType (conArgs datacon)
    ty <- convertScheme scheme
    pure $ Var (Free dataTypeName) ty

getFunDecls :: [Decl_ n e ty] -> [Fun_ n e ty]
getFunDecls = getDeclBy $ \case
  FunDecl f -> Just f
  _         -> Nothing

getDataDecls :: [Decl_ n e ty] -> [Data_ n]
getDataDecls = getDeclBy $ \case
  DataDecl d -> Just d
  _          -> Nothing

getDeclBy :: (Decl_ n e ty -> Maybe a) -> [Decl_ n e ty] -> [a]
getDeclBy _       []         = []
getDeclBy extract (d : rest) = case extract d of
  Just e  -> e : getDeclBy extract rest
  Nothing -> getDeclBy extract rest
