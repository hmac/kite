module Type.Module where

-- Typecheck a Kite module
-- Currently we just return unit (and an updated type env) if the module
-- typechecked. In future we will need to return a copy of the module with full
-- type annotations.

import qualified Data.Set                      as Set
import           Util

import           AST                            ( ConMeta(..) )
import qualified Canonical                     as Can
import           Control.Monad                  ( void )
import qualified Control.Monad.Except          as Except
                                                ( catchError
                                                , throwError
                                                )
import           Data.List.Extra                ( concatUnzip3 )
import           Data.Name
import qualified Syn                           as S
import           Syn                            ( DataCon_(..)
                                                , Data_(..)
                                                , Decl_(..)
                                                , Fun_(..)
                                                , Module_(..)
                                                )
import qualified Syn.Typed                     as T
import           Type                           ( CtorInfo
                                                , Ctx
                                                , CtxElem(V)
                                                , Exp
                                                , LocatedError(..)
                                                , Type(..)
                                                , TypeCtx
                                                , TypeM
                                                , V(..)
                                                , check
                                                , infer
                                                , putCtx
                                                , putTypeCtx
                                                , wellFormedType
                                                )
import           Type.FromSyn                   ( fromSyn
                                                , quantify
                                                )
import qualified Type.ToTyped                   ( convertModule )

-- Translate a module into typechecking structures, and return them
-- Used for debugging
translateModule
  :: Can.Module -> TypeM (Ctx, CtorInfo, [(Name, Maybe Type, Exp)])
translateModule modul = do
  (_, dataTypeCtx, dataTypeInfo) <- concatUnzip3
    <$> mapM translateData (getDataDecls (moduleDecls modul))
  funs <- mapM funToBind $ getFunDecls (moduleDecls modul)
  pure (dataTypeCtx, dataTypeInfo, funs)

-- TODO: return a type-annotated module
-- TODO: check that data type definitions are well-formed
checkModule
  :: (TypeCtx, Ctx, CtorInfo)
  -> Can.Module
  -> TypeM ((TypeCtx, Ctx, CtorInfo), T.Module)
checkModule (typeCtx, ctx, ctorInfo) modul = do
  -- Extract type signatures from all datatype definitions in the module
  (typeNames, dataTypeCtx, dataTypeInfo) <- concatUnzip3
    <$> mapM translateData (getDataDecls (moduleDecls modul))

  -- Get all the functions defined in the module
  funs <- mapM funToBind $ getFunDecls (moduleDecls modul)

  -- Split the functions into those with type signatures and those without
  let (funsWithSig, funsWithoutSig) = flip partitionWith funs $ \case
        (name, Just ty, expr) -> Left (name, ty, expr)
        (name, Nothing, expr) -> Right (name, expr)


  -- Extend the context with type signatures for each function
  -- This allows us to typecheck them in any order, and to typecheck any
  -- recursive calls.
  -- If the function has no type signature (should only happen when we're
  -- invoking this function via the REPL), skip it.
  let funTypeCtx = map (\(name, ty, _exp) -> V (Free name) ty) funsWithSig

  let ctx'       = ctx <> dataTypeCtx <> funTypeCtx
  let ctorInfo'  = ctorInfo <> dataTypeInfo
  let typeCtx'   = map (, ()) typeNames <> typeCtx

  -- Typecheck each function definition
  -- For functions with type signatures, just check them against their signature
  -- For functions without type signatures, infer their type
  putTypeCtx typeCtx'
  putCtx ctx'
  mapM_ (checkFun ctx') funsWithSig
  mapM_ inferFun        funsWithoutSig

  -- Construct a typed module by converting every data & fun decl into the typed
  -- form, with empty type annotations. In the future this should be done during
  -- typechecking itself.
  let typedModule = Type.ToTyped.convertModule ctorInfo' modul

  -- Done
  pure ((typeCtx', ctx', ctorInfo'), typedModule)

checkFun :: Ctx -> (Name, Type, Exp) -> TypeM ()
checkFun ctx (name, ty, body) =
  flip Except.catchError
       (\(LocatedError _ e) -> Except.throwError (LocatedError (Just name) e))
    $ do
  -- check the type is well-formed
        putCtx ctx
        void $ wellFormedType ty
        -- check the body of the function
        void $ check body ty

inferFun :: (Name, Exp) -> TypeM ()
inferFun (name, body) =
  flip Except.catchError
       (\(LocatedError _ e) -> Except.throwError (LocatedError (Just name) e))
    $ do
        -- infer the body of the function
        void $ infer body

funToBind :: Can.Fun Can.Exp -> TypeM (Name, Maybe Type, Exp)
funToBind fun = do
  rhs <- fromSyn (funExpr fun)
  sch <- case funType fun of
    Just t  -> Just <$> quantify (Set.toList (S.ftv t)) t
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
translateData :: Can.Data -> TypeM ([Name], Ctx, CtorInfo)
translateData d = do
  let tyvars = map Local (dataTyVars d)
      info   = zipWith
        (\tag c -> (conName c, ConMeta tag (length (conArgs c)) (dataName d)))
        [0 ..]
        (dataCons d)
  ctx <- mapM (buildCtx (dataName d) tyvars) (dataCons d)
  pure ([dataName d], ctx, info)
 where
  buildCtx :: Name -> [Name] -> Can.DataCon -> TypeM CtxElem
  buildCtx dataTypeName tyvars datacon = do
    -- Construct a (Syn) Scheme for this constructor
    let resultType = foldl S.TyApp (S.TyCon dataTypeName) (map S.TyVar tyvars)
    ty <- quantify tyvars $ foldr S.TyFun resultType (conArgs datacon)
    pure $ V (Free (conName datacon)) ty

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
