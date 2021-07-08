-- Typecheck a Kite module
-- Currently we just return unit (and an updated type env) if the module
-- typechecked. In future we will need to return a copy of the module with full
-- type annotations.
module Type.Module where

import           AST                            ( ConMeta(..) )
import qualified Canonical                     as Can
import           Control.Monad                  ( void )
import qualified Control.Monad.Except          as Except
                                                ( catchError
                                                , throwError
                                                )
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
import           Type                           ( Exp
                                                , LocatedError(..)
                                                , TypecheckM
                                                , check
                                                , infer
                                                , runTypeM
                                                , runTypeMAndSolve
                                                , wellFormedType
                                                , withGlobalCtx
                                                , withGlobalTypeCtx
                                                )
import           Type.FromSyn                   ( fromSyn
                                                , quantify
                                                )
import qualified Type.ToTyped                   ( convertModule )
import           Type.Type                      ( CtorInfo
                                                , Ctx
                                                , CtxElem(V)
                                                , Type(..)
                                                , TypeCtx
                                                )

-- Translate a module into typechecking structures, and return them
-- Used for debugging
translateModule
  :: Can.Module -> TypecheckM (Ctx, CtorInfo, [(Name, Maybe Type, Exp)])
translateModule modul = do
  (_, dataTypeCtx, dataTypeInfo) <- concat3
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
  (typeNames, dataTypeCtx, dataTypeInfo) <- concat3
    <$> mapM translateData (getDataDecls (moduleDecls modul))

  -- Get all the functions defined in the module
  let funs     = getFunDecls (moduleDecls modul)

  let typeCtx' = Map.fromList (map (, ()) typeNames) <> typeCtx

  -- Typecheck each function definition
  funCtx <-
    withGlobalTypeCtx (<> typeCtx')
    $ withGlobalCtx (<> (ctx <> dataTypeCtx))
    $ typecheckFuns funs
  let ctx'        = ctx <> dataTypeCtx <> funCtx

  -- Construct a typed module by converting every data & fun decl into the typed
  -- form, with empty type annotations. In the future this should be done during
  -- typechecking itself.
  let ctorInfo'   = ctorInfo <> dataTypeInfo
  let typedModule = Type.ToTyped.convertModule ctorInfo' modul

  -- Done
  pure ((typeCtx', ctx', ctorInfo'), typedModule)

-- | Typecheck a group of functions which may refer to each other.
typecheckFuns :: [Can.Fun Can.Exp] -> TypecheckM Ctx
typecheckFuns funs = do
  funs' <- mapM (\f -> (f, ) <$> funToBind f) funs

  -- Extend the context with type signatures for each function
  -- This allows us to typecheck them in any order, and to typecheck any
  -- recursive calls.
  -- If the function has no type signature, skip it.
  let funCtx = flip concatMap funs' $ \(_, (name, ty, _)) -> case ty of
        Just t  -> [V name t]
        Nothing -> []

  withGlobalCtx (<> funCtx) $ mapM_ typecheckFun funs'

  pure funCtx

typecheckFun :: (Can.Fun Can.Exp, (Name, Maybe Type, Exp)) -> TypecheckM T.Exp
typecheckFun (fun, (name, mtype, expr)) =
  flip Except.catchError
       (\(LocatedError _ e) -> Except.throwError (LocatedError (Just name) e))
    $ do
        case mtype of
          Just ty -> do
            -- check the type is well-formed
            void $ runTypeM $ wellFormedType ty
          Nothing -> pure ()
        -- check or infer each function in the where clause
        whereCtx <- typecheckFuns (funWheres fun)
        withGlobalCtx (<> whereCtx) $ runTypeMAndSolve $ case mtype of
          -- check the body of the function
          Just ty -> check expr ty
          -- infer the body of the function
          Nothing -> infer expr

funToBind :: Can.Fun Can.Exp -> TypecheckM (Name, Maybe Type, Exp)
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
translateData :: Can.Data -> TypecheckM ([Name], Ctx, CtorInfo)
translateData d = do
  let tyvars = map Local (dataTyVars d)
      info   = zipWith
        (\tag c -> (conName c, ConMeta tag (length (conArgs c)) (dataName d)))
        [0 ..]
        (dataCons d)
  ctx <- mapM (buildCtx (dataName d) tyvars) (dataCons d)
  pure ([dataName d], ctx, Map.fromList info)
 where
  buildCtx :: Name -> [Name] -> Can.DataCon -> TypecheckM CtxElem
  buildCtx dataTypeName tyvars datacon = do
    -- Construct a (Syn) Scheme for this constructor
    let resultType = foldl S.TyApp (S.TyCon dataTypeName) (map S.TyVar tyvars)
    ty <- fmap fst $ runTypeM $ quantify tyvars $ foldr S.TyFun
                                                        resultType
                                                        (conArgs datacon)
    pure $ V (conName datacon) ty

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
