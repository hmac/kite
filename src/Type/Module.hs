module Type.Module where

-- Typecheck a Lam module
-- Currently we just return unit (and an updated type env) if the module
-- typechecked. In future we will need to return a copy of the module with full
-- type annotations.

import qualified Data.Set                      as Set

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
                                                , Exp
                                                , check
                                                , wellFormedType
                                                , throwError
                                                , Error(..)
                                                , LocatedError(..)
                                                )
import           Type.FromSyn                   ( convertScheme
                                                , fromSyn
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Except           ( withExceptT )

-- TODO: return a type-annotated module
-- TODO: check that data type definitions are well-formed
checkModule :: Ctx -> Can.Module -> TypeM (Ctx, Can.Module)
checkModule ctx modul = do
  -- Extract type signatures from all datatype definitions in the module
  dataTypeCtx <- mconcat
    <$> mapM translateData (getDataDecls (moduleDecls modul))

  -- Get all the functions defined in the module
  funs <- mapM funToBind $ getFunDecls (moduleDecls modul)

  -- Extend the context with type signatures for each function
  -- This allows us to typecheck them in any order, and to typecheck any
  -- recursive calls.
  let funTypeCtx = map (\(name, ty, _exp) -> Var (Free name) ty) funs

  let ctx'       = ctx <> dataTypeCtx <> funTypeCtx

  -- Typecheck each function definition
  mapM_ (checkFun ctx') funs

  -- Done
  pure (ctx', modul)

checkFun :: Ctx -> (Name, Type, Exp) -> TypeM ()
checkFun ctx (name, ty, body) =
  withExceptT (\(LocatedError _ e) -> LocatedError (Just name) e) $ do
  -- check the type is well-formed
    void $ wellFormedType ctx ty
    -- check the body of the function
    void $ check ctx body ty

funToBind :: Can.Fun Can.Exp -> TypeM (Name, Type, Exp)
funToBind fun = do
  rhs <- (fromSyn . S.defExpr . head . funDefs) fun
  case funType fun of
    Nothing ->
      throwError $ TodoError $ "function with no type signature: " <> show
        (funName fun)
    Just ty -> do
      sch <- quantify ty
      pure (funName fun, sch, rhs)

-- Explicitly quantify all type variables, then convert the whole thing to a
-- T.Type.
quantify :: Can.Type -> TypeM Type
quantify t =
  let vars = Set.toList (S.ftv t) in convertScheme (S.Forall vars t)

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
