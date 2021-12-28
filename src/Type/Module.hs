-- Typecheck a Kite module
-- Currently we just return unit (and an updated type env) if the module
-- typechecked. In future we will need to return a copy of the module with full
-- type annotations.
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Type.Module where

import           AST                            ( ConMeta(..) )
import qualified Canonical                     as Can
import           Control.Lens                   ( transformMOf
                                                , traverseOf
                                                )
import           Control.Monad                  ( void )
import qualified Control.Monad.Except          as Except
                                                ( catchError
                                                , throwError
                                                )
import           Data.Data.Lens                 ( uniplate )
import           Data.Generics.Sum              ( _Ctor' )
import qualified Data.Map.Strict               as Map
import           Data.Name
import qualified Data.Set                      as Set
import qualified Syn                           as S
import           Syn                            ( DataCon_(..)
                                                , Data_(..)
                                                , Decl_(..)
                                                , Fun_(..)
                                                , Module_(..)
                                                )
import qualified Syn.Typed                     as T
import           Type                           ( Error
                                                  ( MultipleProofsFound
                                                  , NoProofFound
                                                  )
                                                , Exp
                                                , LocatedError(..)
                                                , TypecheckM
                                                , check
                                                , getGlobalCtx
                                                , infer
                                                , runTypeM
                                                , runTypeMAndSolve
                                                , wellFormedType
                                                , withCtorInfo
                                                , withGlobalCtx
                                                , withGlobalTypeCtx
                                                )
import           Type.FromSyn                   ( fromSyn
                                                , quantify
                                                )
import           Type.Type                      ( CtorInfo
                                                , Ctx
                                                , CtxElem(V)
                                                , Type(..)
                                                , TypeCtx
                                                )
import           Util                           ( mapMaybe
                                                , zipWithM
                                                )

-- Translate a module into typechecking structures, and return them
-- Used for debugging
translateModule
  :: Can.Module -> TypecheckM (Ctx, CtorInfo, [(Name, Maybe Type, Exp)])
translateModule modul = do
  (_, dataTypeCtx, dataTypeInfo, _datas) <- concat4
    <$> mapM translateData (getDataDecls (moduleDecls modul))
  funs <- mapM funToBind $ getFunDecls (moduleDecls modul)
  pure (dataTypeCtx, dataTypeInfo, funs)

-- TODO: return a type-annotated module
-- TODO: check that data type definitions are well-formed
checkModule
  :: (TypeCtx, Ctx, CtorInfo)
  -> Can.Module
  -> TypecheckM ((TypeCtx, Ctx, CtorInfo), T.Module)
checkModule (typeCtx, ctx, ctorInfo) modul = do
  -- Extract type signatures from all datatype definitions in the module
  (typeNames, dataTypeCtx, dataTypeInfo, datas) <- concat4
    <$> mapM translateData (getDataDecls (moduleDecls modul))

  -- Get all the functions defined in the module
  let funs      = getFunDecls (moduleDecls modul)

  let typeCtx'  = Map.fromList (map (, ()) typeNames) <> typeCtx

  let ctorInfo' = ctorInfo <> dataTypeInfo

  -- Typecheck each function definition
  (funCtx, funs') <-
    withCtorInfo (<> ctorInfo')
    $ withGlobalTypeCtx (<> typeCtx')
    $ withGlobalCtx (<> (ctx <> dataTypeCtx))
    $ typecheckFuns funs
  let ctx' = ctx <> dataTypeCtx <> funCtx

  let typedModule = T.Module
        { T.moduleName    = moduleName modul
        , T.moduleImports = moduleImports modul
        , T.moduleExports = map fst $ moduleExports modul
        , T.moduleDecls   = map T.FunDecl funs' <> map T.DataDecl datas
        }

  -- Done
  pure ((typeCtx', ctx', ctorInfo'), typedModule)

-- | Typecheck a group of functions which may refer to each other.
typecheckFuns :: [Can.Fun] -> TypecheckM (Ctx, [T.Fun])
typecheckFuns funs = do
  funs' <- mapM (\f -> (f, ) <$> funToBind f) funs

  -- Extend the context with type signatures for each function
  -- This allows us to typecheck them in any order, and to typecheck any
  -- recursive calls.
  -- If the function has no type signature, skip it.
  let funCtx = flip concatMap funs' $ \(_, (name, ty, _)) -> case ty of
        Just t  -> [V name t]
        Nothing -> []

  funs'' <- withGlobalCtx (<> funCtx) $ mapM typecheckFun funs'

  pure (funCtx, funs'')

-- | Typecheck a single function.
typecheckFun :: (Can.Fun, (Name, Maybe Type, Exp)) -> TypecheckM T.Fun
typecheckFun (fun, (name, mtype, expr)) = do
  flip Except.catchError
       (\(LocatedError _ e) -> Except.throwError (LocatedError (Just name) e))
    $ do
        case mtype of
          Just ty -> do
            -- check the type is well-formed
            void $ runTypeM $ wellFormedType ty
          Nothing -> pure ()
        -- check or infer each function in the where clause
        (whereCtx, wheres) <- typecheckFuns (funWheres fun)
        expr' <- withGlobalCtx (<> whereCtx) $ runTypeMAndSolve $ case mtype of
          -- check the body of the function
          Just ty -> check expr ty
          -- infer the body of the function
          Nothing -> infer expr
        -- Resolve any implicits in the function, using the global context only
        -- (so implicit values cannot be defined locally)
        expr'' <- resolveImplicitsInExpr expr'
        pure T.Fun { T.funName   = name
                   , T.funType   = T.typeOf expr''
                   , T.funExpr   = expr''
                   , T.funWheres = wheres
                   }

resolveImplicitsInExpr :: T.Exp -> TypecheckM T.Exp
resolveImplicitsInExpr expr = do
  ctx <- getGlobalCtx
  transformMOf uniplate (traverseOf (_Ctor' @"ImplicitT") (search ctx)) expr
 where
  search :: Ctx -> (T.Type, T.Implicit) -> TypecheckM (T.Type, T.Implicit)
  search ctx (ty, T.Unsolved) = do
    let results = flip mapMaybe ctx $ \case
          V n t | t == ty -> Just n
          _               -> Nothing
    case results of
      []  -> Except.throwError $ LocatedError Nothing (NoProofFound ty)
      [v] -> pure (ty, T.Solved v)
      vs ->
        Except.throwError $ LocatedError Nothing (MultipleProofsFound ty vs)
  search _ r = pure r

funToBind :: Can.Fun -> TypecheckM (Name, Maybe Type, Exp)
funToBind fun = do
  rhs <- fmap fst $ runTypeM $ fromSyn (funExpr fun)
  sch <- case funType fun of
    Just t  -> Just . fst <$> runTypeM (quantify (Set.toList (S.ftv t)) t)
    Nothing -> pure Nothing
  pure (funName fun, sch, rhs)

-- Convert data type definitions into a series of <constructor, type> bindings.
--
--   type Maybe a = Just a | Nothing
-- becomes
--   V (Free "Just") (Forall a. a -> Maybe a)
--   V (Free "Nothing") (Forall a. Maybe a)
--
--   type Functor f = Functor { map : forall a b. (a -> b) -> f a -> f b }
-- becomes
--   V (Free "Functor") (Forall f. { map : Forall a b. (a -> b) -> f a -> f b })
--
translateData :: Can.Data -> TypecheckM ([Name], Ctx, CtorInfo, [T.Data])
translateData d = do
  let tyvars = map Local (dataTyVars d)
      info   = zipWith
        (\tag c -> (conName c, ConMeta tag (length (conArgs c)) (dataName d)))
        [0 ..]
        (dataCons d)
  datacons <- zipWithM
    (\tag c -> do
      t <- ctorType (dataName d) tyvars c
      pure T.DataCon
        { T.conName = conName c
        , T.conType = t
        , T.conMeta = ConMeta { conMetaTag      = tag
                              , conMetaArity    = length (conArgs c)
                              , conMetaTypeName = dataName d
                              }
        }
    )
    [0 ..]
    (dataCons d)
  ctx <- mapM (buildCtx (dataName d) tyvars) (dataCons d)
  let data_ = T.Data { T.dataName   = dataName d
                     , T.dataTyVars = tyvars
                     , T.dataCons   = datacons
                     }
  pure ([dataName d], ctx, Map.fromList info, [data_])
 where
  buildCtx :: Name -> [Name] -> Can.DataCon -> TypecheckM CtxElem
  buildCtx dataTypeName tyvars datacon =
    V (conName datacon) <$> ctorType dataTypeName tyvars datacon
  ctorType :: Name -> [Name] -> Can.DataCon -> TypecheckM Type
  ctorType typeName typeVars datacon =
    let resultType = foldl S.TyApp (S.TyCon typeName) (map S.TyVar typeVars)
    in  fmap fst $ runTypeM $ quantify typeVars $ foldr S.TyFun
                                                        resultType
                                                        (conArgs datacon)

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

-- Like 'Util.concat3' but generalised to any Monoid, not just list
concat3 :: (Monoid a, Monoid b, Monoid c) => [(a, b, c)] -> (a, b, c)
concat3 = foldl go (mempty, mempty, mempty)
  where go (as, bs, cs) (a, b, c) = (a <> as, b <> bs, c <> cs)

-- Like 'Util.concat4' but generalised to any Monoid, not just list
concat4
  :: (Monoid a, Monoid b, Monoid c, Monoid d) => [(a, b, c, d)] -> (a, b, c, d)
concat4 = foldl go (mempty, mempty, mempty, mempty)
  where go (as, bs, cs, ds) (a, b, c, d) = (a <> as, b <> bs, c <> cs, d <> ds)
