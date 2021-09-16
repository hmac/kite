{-# LANGUAGE TemplateHaskell #-}
module Type
  ( test
  , TypecheckM
  , TypeState
  , infer'
  , Exp
  , Pattern
  , Pat(..)
  , unfoldFn
  , foldFn
  , wellFormedType
  , check
  , infer
  , TypeM
  , throwError
  , Error(..)
  , LocatedError(..)
  , checkPattern
  , newU
  , string
  , int
  , char
  , bool
  , unit
  , list
  , runTypeM
  , runTypeMAndSolve
  , defaultTypeEnv
  , TypeEnv(..)
  , subst
  , putCtx
  , putTypeCtx
  , fv
  , quantify
  , withGlobalCtx
  , withGlobalTypeCtx
  , withCtorInfo
  , runTypecheckM
  ) where

import           AST                            ( Expr(..)
                                                , Pat(..)
                                                )
import           Control.Monad                  ( (>=>)
                                                , void
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ask
                                                , asks
                                                , local
                                                )
import           Control.Monad.Writer.Strict    ( WriterT
                                                , runWriterT
                                                , tell
                                                )
import           Data.List                      ( intercalate )
import qualified Data.List.NonEmpty            as NE
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Name                      ( ModuleName(..)
                                                , Name(..)
                                                , RawName(..)
                                                , prim
                                                )
import           Data.Name.Gen                  ( genName )
import           Data.String                    ( fromString )
import           System.Environment             ( lookupEnv )
import           System.IO.Unsafe               ( unsafePerformIO ) -- cheap hack to enable debug mode via env var
import           Util

import           Prelude                 hiding ( splitAt )

import           Data.Maybe                     ( listToMaybe )

import qualified Control.Monad.Except
import           Control.Monad.Except           ( ExceptT(..)
                                                , catchError
                                                , runExceptT
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , runReaderT
                                                )
import           Control.Monad.Trans.State.Strict
                                                ( State
                                                , evalState
                                                , get
                                                , gets
                                                , modify'
                                                , put
                                                , runState
                                                )

import           Hedgehog                       ( (===)
                                                , Gen
                                                , Property
                                                , property
                                                )
import qualified Hedgehog                      as H
import qualified Hedgehog.Gen                  as G
import qualified Hedgehog.Range                as R

import           Syn.Typed                      ( ConMeta(..)
                                                , ExprT(..)
                                                , cacheType
                                                , typeOf
                                                )
import qualified Syn.Typed
import           Type.DSL                       ( e_
                                                , e_'
                                                , fn
                                                , forAll
                                                , tapp
                                                , tcon
                                                , trecord
                                                , u_
                                                , u_'
                                                )
import           Type.Primitive                 ( bool
                                                , char
                                                , fcallInfo
                                                , int
                                                , list
                                                , listConsMeta
                                                , listNilMeta
                                                , primCtx
                                                , primTypeCtx
                                                , primitiveCtorInfo
                                                , string
                                                , unit
                                                )
import           Type.Type                      ( CtorInfo
                                                , Ctx
                                                , CtxElem(..)
                                                , E(..)
                                                , Type(..)
                                                , Type'(..)
                                                , TypeCtx
                                                , U(..)
                                                , debugCtx
                                                )

-- Bidirectional typechecker
-- Following:
-- Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism

type Exp = Expr Name Type
type ExpT = Syn.Typed.Exp

type Pattern = Pat Name

-- | The free (existential) variables of the type
fv :: Type -> [E]
fv = \case
  TApp f as -> fv' f <> concatMap fv as
  TCon _ as -> concatMap fv as
  TOther t  -> fv' t
 where
  fv' :: Type' -> [E]
  fv' = \case
    Fn     a b     -> fv a <> fv b
    IFn    a b     -> fv a <> fv b
    Forall _ a     -> fv a
    EType   e      -> [e]
    UType   _      -> []
    TRecord fields -> concatMap (fv . snd) fields

-- | Quantify the given existential variables by replacing them with forall-bound universal
-- variables.
quantify :: [E] -> Type -> TypeM Type
quantify vars t = do
  -- Generate a fresh UVar for each E
  uMap <- mapM (\e -> (e, ) <$> newU "") vars
  -- Construct a context mapping each E to its replacement UType
  ctx  <- mapM (\(e, u) -> solve e (u_ u)) uMap
  -- Apply the substitution to the type
  let t' = subst' ctx t
  -- Wrap the result in foralls to bind each UType
  pure $ foldr (forAll . snd) t' uMap

-- | Apply the current context, as a substitution, to a type
subst :: Type -> TypeM Type
subst ty = subst' <$> getCtx <*> pure ty

subst' :: Ctx -> Type -> Type
subst' ctx = \case
  -- If the substitution yields a nested TCon or TApp, flatten it back to spine
  -- form.
  TApp f as ->
    let as' = map (subst' ctx) as
    in  case substType' f of
          Right f'            -> TApp f' as'
          Left  (TApp g cs  ) -> TApp g (cs <> as')
          Left  (TOther f'  ) -> TApp f' as'
          Left  (TCon c args) -> TCon c (args <> as')
  TOther t  -> either id TOther $ substType' t
  TCon c as -> TCon c (map (subst' ctx) as)
 where
  substType' :: Type' -> Either Type Type'
  substType' = \case
    UType u        -> pure $ UType u
    Fn     a b     -> pure $ Fn (subst' ctx a) (subst' ctx b)
    IFn    a b     -> pure $ IFn (subst' ctx a) (subst' ctx b)
    Forall u a     -> pure $ Forall u (subst' ctx a)
    TRecord fields -> pure $ TRecord $ mapSnd (subst' ctx) fields
    EType   e      -> substE e ctx
  substE :: E -> Ctx -> Either Type Type'
  substE e = \case
    []                              -> pure $ EType e
    (ESolved e' t : ctx') | e == e' -> Left $ subst' ctx' t
    (EVar e' : _) | e == e'         -> pure $ EType e
    (_ : c)                         -> substE e c

-- | Substitute an existential variable for a universal variable
substEForU :: E -> U -> Type -> Type
substEForU e u = go
 where
  go = \case
    TApp f as -> TApp (go' f) (map go as)
    TCon c as -> TCon c (map go as)
    TOther t  -> TOther $ go' t
  go' = \case
    Fn  a b -> Fn (go a) (go b)
    IFn a b -> IFn (go a) (go b)
    Forall u' a | u == u'   -> Forall u' a
                | otherwise -> Forall u' (go a)
    EType e' -> EType e'
    UType u' | u == u'   -> EType e
             | otherwise -> UType u'
    TRecord fields -> TRecord (mapSnd go fields)

-- | The bound variables in a context
domV :: Ctx -> [Name]
domV = mapMaybe $ \case
  V v _ -> Just v
  _     -> Nothing

-- | The universal variables in a context
domU :: Ctx -> [U]
domU = mapMaybe $ \case
  UVar u -> Just u
  _      -> Nothing

-- | The existential variables in a context
domE :: Ctx -> [E]
domE = mapMaybe $ \case
  EVar e -> Just e
  _      -> Nothing

-- | The markers in a context
markers :: Ctx -> [E]
markers = mapMaybe $ \case
  Marker e -> Just e
  _        -> Nothing

-- Context lookups

-- | Lookup a universal variable in the context
lookupU :: U -> TypeM ()
lookupU u = do
  ctx <- getCtx
  let result = flip firstJust ctx $ \case
        (UVar u') | u' == u -> Just u
        _                   -> Nothing
  case result of
    Nothing -> todoError $ "lookupU failed: " <> show u
    _       -> pure ()

-- | Lookup an existential variable in the context
lookupE :: E -> TypeM ()
lookupE e = do
  ctx <- getCtx
  let result = flip firstJust ctx $ \case
        (EVar e') | e' == e -> Just e
        _                   -> Nothing
  case result of
    Nothing -> todoError $ "lookupE failed: " <> show e
    _       -> pure ()

lookupSolved :: E -> TypeM Type
lookupSolved e = do
  ctx <- getCtx
  let result = flip firstJust ctx $ \case
        (ESolved e' t) | e' == e -> Just (e', t)
        _                        -> Nothing
  case result of
    Nothing      -> todoError "lookupSolved failed"
    Just (_, ty) -> pure ty

-- | Lookup a normal variable in the current context.
-- If it's not in the local context, we check the global context.
lookupV :: Name -> TypeM Type
lookupV v = do
  ctx                            <- getCtx
  TypeEnv { envCtx = globalCtx } <- ask
  liftMaybe (throwError (UnknownVariable v))
    $ flip firstJust (ctx <> globalCtx)
    $ \case
        (V v' t) | v' == v -> Just t
        _                  -> Nothing

-- | Lookup a type in the type context.
-- If it's not in the local context, we check the global context.
lookupType :: Name -> TypeM ()
lookupType name = do
  tctx                               <- getTypeCtx
  TypeEnv { envTypeCtx = globalCtx } <- ask
  case Map.lookup name (tctx <> globalCtx) of
    Just _  -> pure ()
    Nothing -> throwError (UnknownType name)

-- | Lookup info about a constructor in the global constructor info context.
-- If it's not there, throw an error.
lookupCtorInfo :: Name -> TypeM ConMeta
lookupCtorInfo name = do
  TypeEnv { envCtorInfo = info } <- ask
  case Map.lookup name info of
    Just m  -> pure m
    -- TODO: throw 'UnknownConstructor' instead of 'UnknownVariable'
    Nothing -> throwError $ UnknownVariable name

-- | "Solve" an existential by generating an 'ESolved' context element which maps it to a type.
-- We use this to ensure that every time we solve an existential we also write the solution to our
-- output.
solve :: E -> Type -> TypeM CtxElem
solve e t = do
  tell $ Map.singleton e t
  pure $ ESolved e t

-- Context construction

-- | Save the current state, run an action that returns a result,
-- then restore the original state
call :: TypeM a -> TypeM a
call m = do
  st     <- lift $ lift $ lift get
  result <- m
  lift $ lift $ lift $ put st
  pure result

-- | Extend the current context with a universal variable
extendU :: U -> TypeM ()
extendU u = do
  ctx <- getCtx
  if u `elem` domU ctx then todoError "extendU failed" else pushCtx (UVar u)

-- | Extend the current context with a bound variable
extendV :: Name -> Type -> TypeM ()
extendV v t = do
  ctx <- getCtx
  if v `elem` domV ctx
    then todoError $ "extendV failed:" <+> debug v <+> debug t
    else do
      wellFormedType t
      pushCtx (V v t)

-- | Extend the current context with an existential variable
extendE :: E -> TypeM ()
extendE e = do
  ctx <- getCtx
  if e `elem` domE ctx
    then todoError "extendE failed"
    else do
      pushCtx (EVar e)

-- | Extend the current context with a solved existential variable
extendSolved :: E -> Type -> TypeM ()
extendSolved e t = do
  ctx <- getCtx
  if e `elem` domE ctx
    then todoError "extendSolved failed"
    else do
      wellFormedType t
      solution <- solve e t
      pushCtx solution

-- | Extend the current context with an existential marker
extendMarker :: E -> TypeM ()
extendMarker e = do
  ctx <- getCtx
  if e `elem` (domE ctx <> markers ctx)
    then todoError "extendMarker failed"
    else do
      pushCtx (Marker e)

-- | Split a context at a given element, dropping that element
splitAt :: CtxElem -> Ctx -> (Ctx, Ctx)
splitAt e = \case
  []                   -> ([], [])
  (e' : ctx) | e' == e -> ([], ctx)
  (c : ctx)            -> let (l, r) = splitAt e ctx in (c : l, r)

dropAfter :: CtxElem -> TypeM ()
dropAfter e = do
  ctx <- getCtx
  putCtx (dropAfter' e ctx)

-- | Drop all context elements after a particular element, inclusive
dropAfter' :: CtxElem -> Ctx -> Ctx
dropAfter' e = \case
  [] -> []
  (e' : ctx) | e' == e   -> ctx
             | otherwise -> dropAfter' e ctx

-- | Search the current context for variables of the given type.
proofSearch :: Type -> TypeM [Name]
proofSearch ty = do
  localCtx                       <- getCtx
  TypeEnv { envCtx = globalCtx } <- ask
  let ctx = localCtx <> globalCtx
  ty' <- subst ty

  flip mapMaybeM ctx $ \case
    V n t -> do
      -- -- Run subtyping between @t@ and @ty@, but throw away any changes it makes to the state.
      -- call ((subtype t ty $> Just n) `catchError` (const (pure Nothing)))
      t' <- subst t
      pure $ if t' == ty' then Just n else Nothing
    _ -> pure Nothing

-- | Search the context for a unique variable of the given type.
--   Throw an error if no variable is found or if more than one variable is found.
implicitSearch :: Type -> TypeM Name
implicitSearch ty = do
  ty' <- subst ty
  proofSearch ty' >>= \case
    [v] -> pure v
    []  -> throwError $ NoProofFound ty
    vs  -> throwError $ MultipleProofsFound ty vs

-- Check if a type is well-formed
wellFormedType :: Type -> TypeM ()
wellFormedType ty = do
  trace' ["wellFormedType", debug ty] $ case ty of
    TApp f as -> go f >> mapM_ wellFormedType as
    TOther t  -> go t
    -- TODO: check that c has the correct kind (i.e. for all its args)
    TCon c as -> lookupType c >> mapM_ wellFormedType as
 where
  go :: Type' -> TypeM ()
  go = \case
    Fn     a b -> wellFormedType a >> wellFormedType b
    IFn    a b -> wellFormedType a >> wellFormedType b
    Forall u t -> do
      call $ do
        void $ extendU u
        wellFormedType t
    UType u -> lookupU u
    EType e -> do
      lookupE e `catchError` const (void (lookupSolved e))
    TRecord fields -> mapM_ (\(n, t) -> (n, ) <$> wellFormedType t) fields

-- Typechecking monad
data TypeEnv = TypeEnv
  { envCtx      :: Ctx    -- The global type context
  , envTypeCtx  :: TypeCtx -- Global type info (primitive types)
  , envCtorInfo :: CtorInfo -- arity etc. of in-scope constructors
  , envDepth    :: Int  -- The recursion depth, used for debugging
  , envDebug    :: Bool -- Whether to output debug messages
  }
  deriving (Eq, Show)

defaultTypeEnv :: TypeEnv
defaultTypeEnv =
  let debugOn =
        case unsafePerformIO (fmap (== "true") <$> lookupEnv "KITE_DEBUG") of
          Just True -> True
          _         -> False
  in  TypeEnv { envCtx      = primCtx
              , envTypeCtx  = primTypeCtx
              , envCtorInfo = primitiveCtorInfo
              , envDepth    = 0
              , envDebug    = debugOn
              }

-- This type is used outside of this module, to raise errors and set the global context.
-- It also holds a counter for fresh variable names.
type TypecheckM = ReaderT TypeEnv (ExceptT LocatedError (State Int))

runTypecheckM :: TypeEnv -> TypecheckM a -> Either LocatedError a
runTypecheckM env m = evalState (runExceptT (runReaderT m env)) 0

-- This type is used only inside this module. It is like TypecheckM but it can also manipulate the
-- local context.
type TypeM
  = ReaderT
      TypeEnv
      (ExceptT LocatedError (WriterT (Map E Type) (State TypeState)))

-- | Convert a TypeM action into a TypecheckM action.
-- We do this by supplying a default state value, but threading through the variable counter which
-- is present in TypecheckM. This ensures that we can generate fresh variables in both TypeM and
-- TypecheckM and they won't clash.
runTypeM :: TypeM a -> TypecheckM (a, Map E Type)
runTypeM m = do
  c <- lift $ lift get
  let defaultState =
        TypeState { varCounter = c, context = mempty, typeContext = mempty }
  env <- ask
  let m'                          = runReaderT m env
  let m''                         = runExceptT m'
  let m'''                        = runWriterT m''
  let ((m'''', solutions), state) = runState m''' defaultState
  lift $ lift $ put (varCounter state)
  case m'''' of
    Left  err -> lift $ Control.Monad.Except.throwError err
    Right r   -> pure (r, solutions)

-- | Like 'runTypeM' but specalised to returning 'Syn.Typed.Exp'.
-- Before returning, we apply the solution to resolve any existential variables stored in type
-- annotations.
-- The typechecker doesn't currently produce 'Syn.Typed.Exp', but it will in the future.
runTypeMAndSolve :: TypeM Syn.Typed.Exp -> TypecheckM Syn.Typed.Exp
runTypeMAndSolve m = do
  (e, solution) <- runTypeM m
  pure $ Syn.Typed.applySolution solution e

-- Functions for modifying the global type context
-- These are used by other modules, which control the global context.
-- The local context should be managed only by this module.
withGlobalCtx :: MonadReader TypeEnv m => (Ctx -> Ctx) -> m a -> m a
withGlobalCtx f = local (\e -> e { envCtx = f (envCtx e) })

withGlobalTypeCtx
  :: MonadReader TypeEnv m => (TypeCtx -> TypeCtx) -> m a -> m a
withGlobalTypeCtx f = local (\e -> e { envTypeCtx = f (envTypeCtx e) })

withCtorInfo :: MonadReader TypeEnv m => (CtorInfo -> CtorInfo) -> m a -> m a
withCtorInfo f = local (\e -> e { envCtorInfo = f (envCtorInfo e) })

data TypeState = TypeState
  { varCounter  :: Int -- A counter for generating fresh names
  , context     :: Ctx    -- The local type context
  , typeContext :: TypeCtx -- In-scope types
  }

-- Increment the recursion depth
incDepth :: TypeEnv -> TypeEnv
incDepth t = t { envDepth = envDepth t + 1 }

newInt :: TypeM Int
newInt = do
  i <- lift $ lift $ lift $ gets varCounter
  lift $ lift $ lift $ modify' (\st -> st { varCounter = i + 1 })
  pure i

newE :: TypeM E
newE = E <$> newInt

newU :: Name -> TypeM U
newU hint = U <$> newInt <*> pure hint

-- Get the current local context
getCtx :: TypeM Ctx
getCtx = lift $ lift $ lift $ gets context

-- Replace the current local context
putCtx :: Ctx -> TypeM ()
putCtx ctx = lift $ lift $ lift $ modify' $ \st -> st { context = ctx }

-- Push a new element on to the local context
pushCtx :: CtxElem -> TypeM ()
pushCtx e = do
  ctx <- getCtx
  putCtx (e : ctx)

-- | Get the current type context
getTypeCtx :: TypeM TypeCtx
getTypeCtx = lift $ lift $ lift $ gets typeContext

-- | Replace the current type context
putTypeCtx :: TypeCtx -> TypeM ()
putTypeCtx tctx =
  lift $ lift $ lift $ modify' $ \st -> st { typeContext = tctx }

-- | Lift a 'Maybe' value into the 'MaybeT' monad transformer.
liftMaybe :: TypeM a -> Maybe a -> TypeM a
liftMaybe err = maybe err pure

-- Subtyping
-- | Under this input context, the first type is a subtype of the second type,
-- with the given output context.
--
-- Note: this subtyping relation is only about polymorphism: A < B means "A is
-- more polymorphic than B".
-- For example, (∀a. a -> a) < (Int -> Int)
-- Record subtyping is an entirely different matter that we need to model separately (and haven't, yet).
subtype :: Type -> Type -> TypeM ()
subtype typeA typeB =
  subtype' typeA typeB
    `catchError` (\_ -> throwError $ SubtypingFailure typeA typeB)

 where
  subtype' :: Type -> Type -> TypeM ()
  subtype' tA tB = trace' ["subtype", debug tA, debug tB] $ case (tA, tB) of
    -- For subtyping between a type application and a constructor, we need to match the arguments up
    -- from right to left. e.g.
    --
    -- f x y    f   x
    -- | | |    |\  |
    -- T b c    T b c
    --
    -- f ~ T    f ~ T b
    -- x ~ b    x ~ c
    -- y ~ c
    --
    -- We can have fewer application args than constructor args, but not the other way around.
    (TApp v [], TCon c []) -> subtype' (TOther v) (TCon c [])
    (TApp v [], TCon c bs) -> subtype' (TOther v) (TCon c bs)
    (TApp v as, TCon c []) ->
      throwError $ SubtypingFailure (TApp v as) (TCon c [])
    -- 'as' and 'bs' will always be non-empty from here on
    (TApp v [a], TCon c bs) -> do
      subtype' a          (last bs)
      subtype' (TOther v) (TCon c (init bs))
    (TApp v as, TCon c bs) -> do
      let (a, b) = (last as, last bs)
      subtype' a                  b
      subtype' (TApp v (init as)) (TCon c (init bs))

    -- For subtyping between a constructor and a type application, it's the same thing but reversed.
    -- TODO: DRY this up by merging with the above
    (TCon c [], TApp v []) -> subtype' (TCon c []) (TOther v)
    (TCon c bs, TApp v []) -> subtype' (TCon c bs) (TOther v)
    (TCon c [], TApp v as) ->
      throwError $ SubtypingFailure (TApp v as) (TCon c [])
    (TCon c bs, TApp v [a]) -> do
      subtype' (last bs)          a
      subtype' (TCon c (init bs)) (TOther v)
    (TCon c bs, TApp v as) -> do
      let (b, a) = (last bs, last as)
      subtype' b                  a
      subtype' (TCon c (init bs)) (TApp v (init as))

    (TApp v as, TApp u bs) -> do
      subtype' (TOther v) (TOther u)
      mapM_ (uncurry subtype') (zip as bs)
    (TOther (UType a), TOther (UType a')) | a == a' -> lookupU a
    (TOther (EType a), TOther (EType a')) | a == a' -> lookupE a
    (TOther (Fn a1 a2), TOther (Fn b1 b2))          -> do
      subtype' b1 a1
      a2' <- subst a2
      b2' <- subst b2
      subtype' a2' b2'
    (a, TOther (Forall u b)) -> do
      void $ extendU u
      subtype' a b
      dropAfter (UVar u)
    (TOther (Forall u a), b) -> do
      alpha <- newE
      void $ extendMarker alpha
      void $ extendE alpha
      subtype' (substEForU alpha u a) b
      dropAfter (Marker alpha)
    (TOther (EType e), a) | e `elem` fv a -> throwError $ OccursCheck e a
                          | otherwise     -> instantiateL e a
    (a, TOther (EType e)) | e `elem` fv a -> throwError $ OccursCheck e a
                          | otherwise     -> instantiateR e a
    (TCon v as, TCon v' bs) | v == v' ->
      forM_ (zip as bs) (bimapM subst subst >=> uncurry subtype')
    (TOther (TRecord as), TOther (TRecord bs)) -> do
      -- For any records A, B
      -- A < B if A and B have the same set of labels and every field fB in B
      -- has a corresponding field fA in A such that fA < fB
      forM_ bs $ \(label, bTy) -> case lookup label as of
        Nothing  -> throwError $ SubtypingFailure (trecord as) (trecord bs)
        Just aTy -> do
          aTy' <- subst aTy
          bTy' <- subst bTy
          subtype' aTy' bTy'
      forM_ as $ \(label, _) -> case lookup label bs of
        Just _  -> pure ()
        Nothing -> throwError $ SubtypingFailure (trecord as) (trecord bs)

    (a, b) -> do
      a' <- subst a
      b' <- subst b
      throwError $ SubtypingFailure a' b'

-- Instantiation
-- Existential vars are written e, f
-- Types are written a, b
instantiateL :: E -> Type -> TypeM ()
instantiateL = instantiate L

instantiateR :: E -> Type -> TypeM ()
instantiateR = instantiate R

data InstDir = L | R deriving (Eq, Show)

instantiate :: InstDir -> E -> Type -> TypeM ()
instantiate dir e ty = do
  ctx <- getCtx
  trace' ["inst", show dir, debug e, debug ty] $ case ty of
    TOther (EType f) ->
      -- Here we want to instantiate either e := f or f := e. Which one depends
      -- on which variable has a greater scope - i.e. appears first in the
      -- context. We are only allowed to instantiate a variable to another
      -- variable which appears before it in the context.

      -- First, assume that e occurs before f.
      -- Split the context at f, and look for e in the prefix.
      (do
          let (l, r) = splitAt (EVar f) ctx
          call $ do
            putCtx r
            lookupE e
          -- If we find it, instantiate f := e and we're done
          solution <- solve f (TOther (EType e))
          putCtx $ l <> [solution] <> r
        )
        `catchError` (\_ -> do
                       -- e isn't in the prefix, so assume that f occurs before e
                       -- Split the context at e, and look for f in the prefix
                       let (l, r) = splitAt (EVar e) ctx
                       call $ do
                         putCtx r
                         lookupE f
                       -- If we find it, instantiate f := e and we're done.
                       -- Otherwise, throw an error
                       solution <- solve e (e_ f)
                       putCtx $ l <> [solution] <> r
                     )
    TOther (Fn a b) -> do
      let (l, r) = splitAt (EVar e) ctx
      a1       <- newE
      a2       <- newE
      solution <- solve e $ fn (e_ a1) (e_ a2)
      putCtx (l <> [solution, EVar a1, EVar a2] <> r)
      instantiate (flipDir dir) a1 a
      b' <- subst b
      instantiate (flipDir dir) a2 b'
    TOther (Forall u a) -> case dir of
      L -> do
        extendU u
        instantiate dir e a
        dropAfter (UVar u)
      R -> do
        beta <- newE
        extendMarker beta >> extendE beta
        instantiate dir e (substEForU beta u a)
        dropAfter (Marker beta)
    -- hmac: custom rule for type applications
    -- G [e3, e1 = A e3][e2] |- e2 :=< e3   -| G'
    -- ------------------------------------------
    -- G[e1][e2]             |- A e2 <=: e1 -| G'
    --
    -- TODO: check that this doesn't have any nasty edge cases!
    TCon c args -> do
      let (l, r) = splitAt (EVar e) ctx
      -- generate new Es for each arg
      es       <- mapM (const newE) args
      -- insert them into the context before e, along with a solution e = TCon c es
      solution <- solve e $ tcon c $ map e_ es
      putCtx $ l <> [solution] <> map EVar es <> r
      -- instantiate each arg to its corresponding e
      mapM_ (uncurry $ instantiate (flipDir dir)) (zip es args)

    -- We do the same for type applications
    -- TODO: does this work if f contains existential vars?
    TApp f args -> do
      let (l, r) = splitAt (EVar e) ctx
      -- generate new Es for each arg
      es       <- mapM (const newE) args
      -- insert them into the context before e, along with a solution e = f es
      solution <- solve e $ tapp f $ map e_ es
      putCtx $ l <> [solution] <> map EVar es <> r
      -- instantiate each arg to its corresponding e
      mapM_ (uncurry $ instantiate (flipDir dir)) (zip es args)
    a -> do
      let (l, r) = splitAt (EVar e) ctx
      call $ do
        putCtx r
        wellFormedType a
      solution <- solve e a
      putCtx $ l <> [solution] <> r
 where
  flipDir :: InstDir -> InstDir
  flipDir L = R
  flipDir R = L

checkAbs :: NonEmpty Name -> Exp -> Type -> TypeM (NonEmpty (Name, Type), ExpT)
checkAbs args body (TOther (Fn a b)) | (x, Nothing) <- NE.uncons args = do
  void $ extendV x a
  body' <- check body b
  dropAfter $ V x a
  pure ((x, a) :| [], body')
checkAbs args body (TOther (Fn a b)) | (x, Just xs) <- NE.uncons args = do
  void $ extendV x a
  (xs', body') <- checkAbs xs body b
  dropAfter $ V x a
  pure (NE.cons (x, a) xs', body')
-- We expect this to error, but instead of throwing a custom error for "lambda should have function
-- type but does not", we just infer the true type of the lambda throw a subtyping error between the
-- two types.
checkAbs args body b = do
  a  <- infer (Abs args body) >>= subst . typeOf
  b' <- subst b
  throwError $ SubtypingFailure a b'

-- Typing
{-# ANN check ("HLint: ignore Reduce duplication" :: String) #-}
check :: Exp -> Type -> TypeM ExpT
check expr ty = do
  trace' ["check", debug expr, ":", debug ty] $ case (expr, ty) of

    (e, TOther (Forall u a)) -> do
      extendU u
      e' <- check e a
      dropAfter (UVar u)
      -- Re-add the forall by caching the input type on the expression
      pure $ cacheType ty e'

    -- We've encountered an implicit function type, so we must try to fill it in.
    (e, TOther (IFn a b)) -> do
      alpha <- newE
      void $ extendMarker alpha
      extendSolved alpha a

      -- Check e : b under the assumption that alpha : A is in scope.
      e' <- check e b

      dropAfter (Marker alpha)

      -- Return a normal lambda which expects a value of type A as its first argument.
      -- TODO: generate a fresh variable name properly
      pure $ IAbsT (TOther (Fn a b)) "fixme" a e'

    (Abs args e, _) -> do
      (args', e') <- checkAbs args e ty
      pure $ AbsT ty args' e'

    (Hole n        , a) -> throwError $ CannotCheckHole (Hole n) a
    (Let binds body, _) -> do
      -- generate a dummy existential that we'll use to cut the context
      alpha <- newE
      void $ extendMarker alpha

      -- if the binding is annotated, check it against its annotation
      -- otherwise infer a type for it
      -- then add the type to the context
      binds' <- forM binds $ \(x, e, maybeType) -> do
        e' <- case maybeType of
          Just t  -> check e t
          Nothing -> infer e
        extendV x (typeOf e')
        pure (x, e', maybeType)

      -- check the type of the body
      body' <- check body ty

      -- drop all these variables off the context
      dropAfter (Marker alpha)
      pure $ LetT ty binds' body'
    (FCall name args, _) -> do
      case Map.lookup name fcallInfo of
        Just fCallTy -> do
          -- Construct a temporary 'FCallT' annotated with the type of the
          -- foreign call in order to feed it to 'inferApp'.
          let fcall = FCallT fCallTy name []
          (result, args') <- mapAccumLM
            (\r a -> do
              (rTy, r', a') <- inferApp2 r a
              pure (AppT rTy r' a', a')
            )
            fcall
            args
          resultTy <- subst (typeOf result)
          void $ subtype resultTy ty
          pure $ FCallT resultTy name args'
        Nothing -> throwError $ UnknownFCall name
    (MCase []                  , _              ) -> throwError (EmptyCase expr)
    (MCase alts@((pats, _) : _), TOther (Fn a b)) -> do
      -- Split off as many args as there are patterns in the first alt
      let (argTys, exprTy) =
            let (as, b') = unfoldFn (fn a b)
            in  (take (length pats) as, foldFn (drop (length pats) as) b')
      alts' <- mapM (checkMCaseAlt argTys exprTy) alts
      pure $ MCaseT ty alts'
    (Case scrut alts, _) -> do
      scrut' <- infer scrut
      alts'  <- mapM (checkCaseAlt ty (typeOf scrut')) alts
      pure $ CaseT ty scrut' alts'
    (e, b) -> do
      e' <- infer e
      a  <- subst $ typeOf e'
      b' <- subst b
      void $ subtype a b'
      pure $ cacheType b' e'

checkMCaseAlt :: [Type] -> Type -> ([Pattern], Exp) -> TypeM ([Pattern], ExpT)
checkMCaseAlt patTys _ (pats, _) | length patTys /= length pats =
  throwError TooManyPatterns
checkMCaseAlt patTys rhsTy (pats, rhs) = do
  -- generate a dummy existential that we'll use as a marker to know where to
  -- split the context, at the end of this function
  alpha <- newE
  void $ extendMarker alpha

  -- check each pat against the corresponding pat type, accumulating a new
  -- context
  mapM_ (uncurry checkPattern) (zip pats patTys)

  -- check the rhs against the rhs type
  rhs' <- check rhs rhsTy

  -- drop all context elements after and including the marker
  dropAfter (Marker alpha)

  pure (pats, rhs')

-- Like infer but applies the resulting substitution to the type and returns
-- just the type.
-- Useful for tests.
infer' :: Exp -> TypeM Type
infer' e = trace' ["infer'", debug e] $ do
  e' <- infer e
  subst (typeOf e')

inferAbs :: NonEmpty Name -> Exp -> TypeM (NonEmpty (Name, Type), ExpT, Type)
inferAbs args body = case NE.uncons args of
  (x, Nothing) -> do
    alpha <- newE
    beta  <- newE
    extendE alpha >> extendE beta >> extendV x (e_ alpha)
    body' <- check body (e_ beta)
    dropAfter $ V x (e_ alpha)
    ty <- subst $ fn (e_ alpha) (e_ beta)
    pure ((x, e_ alpha) :| [], body', ty)
  (x, Just xs) -> do
    alpha <- newE
    beta  <- newE
    extendE alpha >> extendE beta >> extendV x (e_ alpha)
    (xs', body', t) <- inferAbs xs body
    subtype t (e_ beta)
    dropAfter $ V x (e_ alpha)
    ty <- subst $ fn (e_ alpha) (e_ beta)
    pure (NE.cons (x, e_ alpha) xs', body', ty)

infer :: Exp -> TypeM ExpT
infer expr_ = do
  trace' ["infer", debug expr_] $ case expr_ of
    Var x   -> VarT <$> lookupV x <*> pure x
    Ann e a -> do
      void $ wellFormedType a
      check e a
    App e1 e2 -> do
      e1'            <- infer e1
      (t, e1'', e2') <- inferApp2 e1' e2
      t'             <- subst t
      pure $ AppT t' e1'' e2'
      -- subst (typeOf e1') >>= \case
      --   -- If 'e1' has an implicit function type, we need to resolve that before we can infer the
      --   -- application. We do that by searching the context for a value to fill it with.
      --   -- TODO: can we get rid of this now we handle implicits in 'check'?
      --   TOther (IFn a b) -> do
      --     valueA    <- implicitSearch a
      --     (b', e2') <- inferApp b e2
      --     b''       <- subst b'
      --     pure $ AppT b'' (AppT b e1' (VarT a valueA)) e2'
      --   ty -> do
      --     (b, e2') <- inferApp ty e2
      --     b'       <- subst b
      --     pure $ AppT b' e1' e2'
    Abs args e -> do
      (args', e', ty) <- inferAbs args e
      pure $ AbsT ty args' e'
    Hole n -> throwError $ CannotInferHole (Hole n)
    Con  x -> do
      t    <- lookupV x
      meta <- lookupCtorInfo x
      pure $ ConT t x meta
    c@(  Case _ [])            -> throwError $ EmptyCase c
    Case scrut    (alt : alts) -> do
      scrut' <- infer scrut
      let scrutTy = typeOf scrut'
      (pat, expr') <- inferCaseAlt scrutTy alt
      let altTy = typeOf expr'
      -- altTy might have foralls in it, for example if alt is []
      -- the other alts may have valid concrete types like [Int],
      -- and if we blindly check that they are subtypes of [] then we'll raise a
      -- type error.
      -- Instead, we strip off all the foralls, generating existentials for each
      -- one, and then check the alts.
      altTy' <- existentialiseOuterForalls altTy
      alts'  <- mapM (checkCaseAlt altTy' scrutTy) alts
      pure $ CaseT altTy' scrut' ((pat, expr') : alts')
    c@(MCase [])                -> throwError $ EmptyCase c
    MCase ((pats, expr) : alts) -> do
      -- The type of an mcase will be a function type taking as many arguments as
      -- there are patterns. Each alt should have the same number of patterns.
      -- Taking the first alt, we infer a type for each pattern and a type for the
      -- RHS expr. We then check the remaining alts against this. This yields a
      -- function type cache on the returned expression. It may have existential
      -- variables that need to be resolved later on.

      -- First, we infer a type for each pattern in the first alt
      (pats', patTys) <- unzip <$> mapM inferPattern pats
      -- Next, infer a type for the RHS
      expr'           <- infer expr
      let exprTy = typeOf expr'
      -- Now check the remaining alts using this information
      alts' <- mapM (checkMCaseAlt patTys exprTy) alts
      -- Now construct a result type and return it
      let ty = foldFn patTys exprTy
      pure $ MCaseT ty ((pats', expr') : alts')
    Let binds body -> do
      -- generate a dummy existential that we'll use to cut the context
      alpha <- newE
      void $ extendMarker alpha

      -- if the binding is annotated, check it against its annotation
      -- otherwise infer a type for it
      -- then add the type to the context
      binds' <- forM binds $ \(x, e, maybeType) -> do
        e' <- case maybeType of
          Just t  -> check e t
          Nothing -> infer e
        extendV x (typeOf e')
        pure (x, e', maybeType)

      -- infer the type of the body
      body' <- infer body

      -- drop all these variables off the context
      ty'   <- subst $ typeOf body'
      dropAfter (Marker alpha)
      pure $ LetT ty' binds' body'
    StringInterp prefix comps -> do
      -- Construct a nested application of appendString and infer the type of that
      let append = Var (prim "appendString")
          expr   = foldl
            (\acc (c, s) -> App (App append acc) (App (App append c) s))
            (StringLit prefix)
            (mapSnd StringLit (NE.toList comps))
      infer expr
    StringLit s   -> pure $ StringLitT string s
    CharLit   c   -> pure $ CharLitT char c
    IntLit    i   -> pure $ IntLitT int i
    BoolLit   b   -> pure $ BoolLitT bool b
    UnitLit       -> pure $ UnitLitT unit
    ListLit elems -> do
      -- Construct a nested application of (::) and [] and infer the type of that
      let expr = foldr (App . App (Con (prim "::"))) (Con (prim "[]")) elems
      infer expr
    TupleLit elems -> do
      -- Construct an application of TupleN and infer the type of that
      let n    = length elems
          con  = Con $ prim $ fromString $ "Tuple" <> show n
          expr = foldl App con elems
      infer expr
    Record fields -> do
      -- To infer the type of a record, we must be able to infer types for all its
      -- fields
      fields' <- mapM (secondM infer) fields
      let ty = trecord $ mapSnd typeOf fields'
      pure $ RecordT ty fields'
    Project record fieldName -> do
      -- To infer the type of a record projection, we must know the type of the
      -- record.
      record' <- infer record
      -- Check if the record contains this field
      ty      <- subst (typeOf record') >>= \case
        r@(TOther (TRecord fields)) -> case lookup fieldName fields of
          Just fieldTy -> pure fieldTy
          Nothing      -> throwError $ RecordDoesNotHaveField r fieldName
        t -> throwError $ NotARecordType t
      pure $ ProjectT ty record' fieldName
    e@(FCall _name _args) -> throwError $ CannotInferFCall e

-- Strip off all the outer foralls from a type, replacing them with
-- existentials.
-- [] forall a b c. t ==> [e1 e2 e3] [e3/c][e2/b][e1/a]t
existentialiseOuterForalls :: Type -> TypeM Type
existentialiseOuterForalls (TOther (Forall u a)) = do
  alpha <- newE
  extendE alpha
  existentialiseOuterForalls (substEForU alpha u a)
existentialiseOuterForalls t = pure t

checkCaseAlt :: Type -> Type -> (Pattern, Exp) -> TypeM (Pattern, ExpT)
checkCaseAlt expectedAltTy scrutTy (pat, expr) = do
  trace'
      [ "checkCaseAlt"
      , debug scrutTy
      , debug expectedAltTy
      , debug pat
      , debug expr
      ]
    $ do
        pat'  <- checkPattern pat scrutTy
        expr' <- check expr expectedAltTy
        pure (pat', expr')

-- TODO: probably better to infer the alts first, since they often constrain the
-- scrut type, and then we don't need to infer it.
inferCaseAlt :: Type -> (Pattern, Exp) -> TypeM (Pattern, ExpT)
inferCaseAlt scrutTy (pat, expr) = do
  trace' ["inferCaseAlt", debug scrutTy, debug pat, debug expr] $ do
    pat'  <- checkPattern pat scrutTy
    expr' <- infer expr
    pure (pat', expr')

{-# ANN inferPattern ("HLint: ignore Reduce duplication" :: String) #-}
inferPattern :: Pattern -> TypeM (Pattern, Type)
inferPattern pat = trace' ["inferPattern", debug pat] $ case pat of
  IntPat  _                 -> pure (pat, int)
  CharPat _                 -> pure (pat, char)
  BoolPat _                 -> pure (pat, bool)
  UnitPat                   -> pure (pat, unit)
  StringPat _               -> pure (pat, string)
  ConsPat con _meta subpats -> do
    -- Lookup the type of the constructor
    conTy <- lookupV con
    case second unfoldFn (unfoldForall conTy) of
      (us, (argTys, tycon@TCon{})) -> do
        -- Create new existentials for each universal in the forall
        -- Add them to the context
        eSub <- mapM (\u -> (, u) <$> newE) us
        mapM_ (extendE . fst) eSub

        -- Substitute them into the argtys and the tycon
        let argTys' = map
              (\t -> foldl (\t' (e, u) -> substEForU e u t') t eSub)
              argTys
            tycon' = foldl (\t' (e, u) -> substEForU e u t') tycon eSub

        -- Check each subpattern against the corresponding argty
        subpats' <- zipWithM checkPattern subpats argTys'

        -- Lookup the metadata of the constructor
        meta     <- lookupCtorInfo con

        -- Return the constructor type
        pure (ConsPat con (Just meta) subpats', tycon')
      (_, (_, t)) -> throwError $ ExpectedConstructorType t
  VarPat x -> do
    alpha <- newE
    extendE alpha >> extendV x (e_ alpha)
    pure (pat, e_ alpha)
  WildPat -> do
    alpha <- newE
    extendE alpha
    pure (pat, e_ alpha)
  TuplePat subpats ->
    let con = case subpats of
          []                       -> error "Type.inferPattern: empty tuple"
          [_] -> error "Type.inferPattern: single-element tuple"
          [_, _]                   -> prim "Tuple2"
          [_, _, _]                -> prim "Tuple3"
          [_, _, _, _]             -> prim "Tuple4"
          [_, _, _, _, _]          -> prim "Tuple5"
          [_, _, _, _, _, _]       -> prim "Tuple6"
          [_, _, _, _, _, _, _]    -> prim "Tuple7"
          [_, _, _, _, _, _, _, _] -> prim "Tuple8"
          _ ->
            error
              $  "Type.inferPattern: cannot (yet) handle tuples of length > 8: "
              <> show subpats
    in  inferPattern (ConsPat con Nothing subpats)
  ListPat []      -> inferPattern (ConsPat (prim "[]") (Just listNilMeta) [])
  ListPat subpats -> inferPattern
    (foldr (\s acc -> ConsPat (prim "::") (Just listConsMeta) [s, acc])
           (ConsPat (prim "[]") (Just listNilMeta) [])
           subpats
    )

checkPattern :: Pattern -> Type -> TypeM Pattern
checkPattern pat ty = do
  trace' ["checkPattern", debug pat, ":", debug ty] $ case pat of
    WildPat  -> pure pat
    VarPat x -> do
      ctx <- getCtx
      if x `elem` domV ctx
        then throwError (DuplicateVariable x)
        else extendV x ty >> pure pat
    IntPat  _                 -> subtype ty int >> pure pat
    CharPat _                 -> subtype ty char >> pure pat
    BoolPat _                 -> subtype ty bool >> pure pat
    UnitPat                   -> subtype ty unit >> pure pat
    StringPat _               -> subtype ty string >> pure pat
    ConsPat con _meta subpats -> do
      constructorType <- lookupV con
      case second unfoldFn (unfoldForall constructorType) of
        (us, (argTys, tycon@TCon{})) -> do
          -- Create new existentials for each universal in the forall
          eSub <- mapM (\u -> (, u) <$> newE) us

          -- Add new existentials to the context
          mapM_ (extendE . fst) eSub

          -- Substitute them into the argtys and the tycon
          let
            argTys' =
              map (\t -> foldl (\t' (e, u) -> substEForU e u t') t eSub) argTys
            tycon' = foldl (\t' (e, u) -> substEForU e u t') tycon eSub

          -- Check each subpattern against the corresponding argty
          subpats' <- zipWithM checkPattern subpats argTys'

          -- Check this against the type we have been given
          tycon''  <- subst tycon'
          ty'      <- subst ty
          subtype tycon'' ty'

          -- Lookup the metadata of the constructor
          meta <- lookupCtorInfo con

          pure $ ConsPat con (Just meta) subpats'
        (_, (_, t)) -> throwError $ ExpectedConstructorType t
    ListPat [] -> checkPattern (ConsPat (prim "[]") (Just listNilMeta) []) ty
    ListPat subpats -> checkPattern
      (foldr (\s acc -> ConsPat (prim "::") (Just listConsMeta) [s, acc])
             (ConsPat (prim "[]") (Just listNilMeta) [])
             subpats
      )
      ty
    TuplePat subpats ->
      let
        con = case subpats of
          []                       -> error "Type.checkPattern: empty tuple"
          [_] -> error "Type.checkPattern: single-element tuple"
          [_, _]                   -> prim "Tuple2"
          [_, _, _]                -> prim "Tuple3"
          [_, _, _, _]             -> prim "Tuple4"
          [_, _, _, _, _]          -> prim "Tuple5"
          [_, _, _, _, _, _]       -> prim "Tuple6"
          [_, _, _, _, _, _, _]    -> prim "Tuple7"
          [_, _, _, _, _, _, _, _] -> prim "Tuple8"
          _ ->
            error
              $  "Type.checkPattern: cannot (yet) handle tuples of length > 8: "
              <> show subpats
      in  checkPattern (ConsPat con Nothing subpats) ty

-- | unfoldForall (Forall a (Forall b t)) == ([a, b], t)
unfoldForall :: Type -> ([U], Type)
unfoldForall (TOther (Forall u t)) =
  let (us, t') = unfoldForall t in (u : us, t')
unfoldForall t = ([], t)

-- | unfoldFn (Fn a (Fn b (Fn c d))) = ([a, b, c], d)
unfoldFn :: Type -> ([Type], Type)
unfoldFn (TOther (Fn a b)) = let (ts, t) = unfoldFn b in (a : ts, t)
unfoldFn t                 = ([], t)

-- | foldFn [a, b, c] d = (Fn a (Fn b (Fn c d)))
foldFn :: [Type] -> Type -> Type
foldFn []       t = t
foldFn [a     ] t = fn a t
foldFn (a : as) t = fn a (foldFn as t)

-- | Infer the result when applying an already-typechecked expression @e1@ to @e2@.
-- Returns a tuple @(t, a, b)@ of the type, lhs and rhs of the resultant application.
-- This is so you can split it back out again if you need to (e.g. see fcall)
inferApp2 :: ExpT -> Exp -> TypeM (Type, ExpT, ExpT)
inferApp2 e1 e2 = do
  t1 <- subst $ typeOf e1
  trace' ["inferApp", debug e1, debug t1, debug e2] $ case t1 of
    TOther (Forall u a) -> do
      alpha <- newE
      extendE alpha
      let t1' = substEForU alpha u a
      inferApp2 (cacheType t1' e1) e2
    TOther (EType alpha) -> do
      a1 <- newE
      a2 <- newE
      extendE a2 >> extendE a1 >> extendSolved alpha (fn (e_ a1) (e_ a2))
      e2' <- check e2 (e_ a1)
      pure (e_ a2, e1, e2')
    TOther (Fn a b) -> do
      e2' <- check e2 a
      pure (b, e1, e2')
    TOther (IFn a b) -> do
      -- Run 'inferApp' to resolve any existentials required to find a proof for this implicit.
      -- This _won't_ elaborate any further implicits present in 'b'.
      _      <- inferApp b e2
      valueA <- implicitSearch a
      a'     <- subst a
      trace' ["proof found:", debug valueA, debug a'] $ pure ()

      -- Now we have a proof for 'a', run 'inferApp2' to properly elaborate any further implicits in
      -- 'e1'.
      let e1' = AppT b e1 (VarT a valueA)
      (t', e21, e22) <- inferApp2 e1' e2
      pure (t', e1', AppT t' e21 e22)

    _ -> throwError $ InfAppFailure t1 e2

-- Infer the type of the result when applying a function of type @ty@ to @e@
-- Return the type of the result, along with a type-annotated @e@.
inferApp :: Type -> Exp -> TypeM (Type, ExpT)
inferApp ty e = do
  trace' ["inferApp", debug ty, debug e] $ case ty of
    TOther (Forall u a) -> do
      alpha <- newE
      extendE alpha
      inferApp (substEForU alpha u a) e
    TOther (EType alpha) -> do
      a1 <- newE
      a2 <- newE
      extendE a2 >> extendE a1 >> extendSolved alpha (fn (e_ a1) (e_ a2))
      e' <- check e (e_ a1)
      pure (e_ a2, e')
    TOther (Fn a b) -> do
      e' <- check e a
      pure (b, e')
    TOther (IFn _a b) -> inferApp b e
    _                 -> throwError $ InfAppFailure ty e

-- A type error, optionally with the name of the function definition that caused
-- it.
data LocatedError = LocatedError (Maybe Name) Error
  deriving (Eq, Show)

-- Type errors
data Error = TodoError String
           | OccursCheck E Type
           | OtherError Error
           | SubtypingFailure Type Type
           | InfAppFailure Type Exp
           | InfAppFailure' Type Type
           | CannotInferHole Exp
           | CannotCheckHole Exp Type
           | UnknownVariable Name
           | UnknownFCall String
           | EmptyCase Exp
           | ExpectedConstructorType Type
           | RecordDoesNotHaveField Type String
           | NotARecordType Type
           | CannotInferFCall Exp
           | TooManyPatterns
           | DuplicateVariable Name
           | UnknownType Name
           | NoProofFound Type
           | MultipleProofsFound Type [Name]
           deriving (Eq, Show)

todoError :: String -> TypeM a
todoError = throwError . TodoError

throwError :: Error -> TypeM a
throwError err = Control.Monad.Except.throwError (LocatedError Nothing err)

-- Util functions
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

-- Like trace, but specifically for recursive calls in TypeM
-- If debugging is enabled, prints the given args, indented by the recursion
-- depth (which it increments), and the context.
trace' :: [String] -> TypeM a -> TypeM a
trace' args m = do
  ctx       <- getCtx
  i         <- asks envDepth
  debugging <- asks envDebug
  let indent = replicate i '|'
      msg    = indent <+> debugCtx ctx <+> sepBy " " args
  if debugging then trace msg (local incDepth m) else local incDepth m

(<+>) :: String -> String -> String
a <+> b = a <> " " <> b

sepBy :: String -> [String] -> String
sepBy = intercalate

-- Property tests

-- TODO: records
genType :: Gen Type
genType = G.recursive
  G.choice
  [e_ <$> genE, u_ <$> genU]
  [ G.subterm2 genType genType fn
  , G.subtermM genType (\t -> forAll <$> genU <*> pure t)
  , tcon <$> genName <*> G.list (R.linear 0 3) genType
  ]

genCtxElem :: Gen CtxElem
genCtxElem = G.choice
  [ V <$> genName <*> genType
  , UVar <$> genU
  , EVar <$> genE
  , ESolved <$> genE <*> genType
  , Marker <$> genE
  ]

genCtx :: Gen Ctx
genCtx = G.list (R.linear 1 10) genCtxElem

genU :: Gen U
genU = U <$> G.int (R.linear 0 100) <*> genName

genE :: Gen E
genE = E <$> G.int (R.linear 0 100)

genModuleName :: Gen ModuleName
genModuleName = ModuleName <$> G.list (R.linear 1 3) genUpperString

genUpperString :: Gen String
genUpperString = do
  c  <- G.upper
  cs <- G.list (R.linear 0 10) G.alphaNum
  pure (c : cs)

genUpperRawName :: Gen RawName
genUpperRawName = Name <$> genUpperString

test :: H.Group
test = $$(H.discover)

prop_fv_commutes :: Property
prop_fv_commutes = property $ do
  a <- H.forAll genType
  b <- H.forAll genType
  fv (fn a b) === (fv a <> fv b)

prop_subst_commutes :: Property
prop_subst_commutes = property $ do
  ctx <- H.forAll genCtx
  a   <- H.forAll genType
  b   <- H.forAll genType
  subst' ctx (fn a b) === fn (subst' ctx a) (subst' ctx b)

prop_substEForU_commutes :: Property
prop_substEForU_commutes = property $ do
  e <- H.forAll genE
  u <- H.forAll genU
  a <- H.forAll genType
  b <- H.forAll genType
  substEForU e u (fn a b) === fn (substEForU e u a) (substEForU e u b)

typecheckTest :: Ctx -> TypeM ExpT -> Either LocatedError Type
typecheckTest ctx =
  second typeOf
    . fmap fst
    . runTypecheckM defaultTypeEnv
    . withGlobalCtx (<> ctx)
    . runTypeM

prop_uval_infers_unit :: Property
prop_uval_infers_unit = property $ do
  ctx <- H.forAll genCtx
  let expr = UnitLit
      ty   = TCon (prim "Unit") []
  typecheckTest ctx (infer expr) === Right ty

prop_checks_bad_unit_annotation :: Property
prop_checks_bad_unit_annotation = property $ do
  ctx <- H.forAll genCtx
  let expr = UnitLit
      ty   = fn unit unit
  typecheckTest ctx (check expr ty)
    === Left (LocatedError Nothing (SubtypingFailure unit (fn unit unit)))

prop_infers_simple_app :: Property
prop_infers_simple_app = property $ do
  v <- H.forAll genName
  let uval = UnitLit
  let expr = App (Ann (Abs (NE.fromList [v]) uval) (fn unit unit)) uval
  typecheckTest mempty (infer expr) === Right unit

prop_infers_app_with_context :: Property
prop_infers_app_with_context = property $ do
  -- The context contains id : Unit -> Unit
  let uval = UnitLit
      ctx  = [V "id" (fn unit unit)]
  -- The expression is id Unit
  let expr = App (Var "id") uval
  -- The inferred type should be Unit
  typecheckTest ctx (infer expr) === Right unit

prop_infers_polymorphic_app :: Property
prop_infers_polymorphic_app = property $ do
  -- The context contains id     : forall a. a -> a
  --                      idUnit : Unit -> Unit
  let uval = UnitLit
      a    = U 0 "a"
      ctx  = [V "id" (forAll a (fn (u_ a) (u_ a))), V "idUnit" (fn unit unit)]
  -- The expression is idUnit (id Unit)
  let expr = App (Var "idUnit") (App (Var "id") uval)
  -- The inferred type should be Unit
  typecheckTest ctx (infer expr) === Right unit

prop_infers_list_app :: Property
prop_infers_list_app = property $ do
  let a    = U 0 "a"
      ty   = forAll a (list (u_ a))
      -- [] : forall a. List a
      ctx  = [V "[]" ty]
      expr = Var "[]"
  typecheckTest ctx (infer expr) === Right ty

prop_infers_bool_case :: Property
prop_infers_bool_case = property $ do
  let
      -- case True of
      --   True -> ()
      --   False -> ()
    expr1 =
      Case (BoolLit True) [(BoolPat True, UnitLit), (BoolPat False, UnitLit)]
    -- case True of
    --   True -> True
    --   False -> ()
    expr2 = Case (BoolLit True)
                 [(BoolPat True, BoolLit True), (BoolPat False, UnitLit)]
  typecheckTest mempty (infer expr1) === Right unit
  typecheckTest mempty (infer expr2)
    === Left (LocatedError Nothing (SubtypingFailure unit bool))

prop_checks_higher_kinded_application :: Property
prop_checks_higher_kinded_application = property $ do
  -- (\x -> x) : f a -> f a should check
  let
    f     = U 1 "f"
    a     = U 2 "a"
    expr1 = Abs (NE.fromList ["x"]) (Var "x")
    type1 =
      forAll f (forAll a (fn (tapp (u_' f) [u_ a]) (tapp (u_' f) [u_ a])))
  typecheckTest mempty (check expr1 type1) === Right type1
  -- ((\x -> x) : f a -> f a) [1] : [Int] should check
  -- ((\x -> x) : f a -> f a) [1]         should infer [Int]
  let expr2 = App (Ann expr1 type1)
                  (App (App (Var (prim "::")) (IntLit 1)) (Var (prim "[]")))
      type2 = list int
  typecheckTest mempty (check expr2 type2) === Right type2
  typecheckTest mempty (infer expr2) === Right (list int)
  -- ((\x -> x) : f a -> f a) True : [Int] should fail to check
  -- ((\x -> x) : f a -> f a) True         should fail to infer
  let expr3 = App (Ann expr1 type1) (Var (prim "True"))
      type3 = list int
  typecheckTest mempty (check expr3 type3) === Left
    (LocatedError
      Nothing
      (SubtypingFailure (tcon (prim "Bool") []) (tapp (e_' (E 0)) [e_ (E 1)]))
    )
  typecheckTest mempty (infer expr3) === Left
    (LocatedError
      Nothing
      (SubtypingFailure (tcon (prim "Bool") []) (tapp (e_' (E 0)) [e_ (E 1)]))
    )
