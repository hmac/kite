{-# LANGUAGE TemplateHaskell #-}
module Type
  ( CtorInfo
  , Ctx
  , TypeCtx
  , test
  , TypecheckM
  , TypeState
  , infer'
  , CtxElem(..)
  , Type(..)
  , Exp
  , V(..)
  , U(..)
  , E(..)
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
  , mkTupleCon
  , primCtx
  , runTypeM
  , defaultTypeEnv
  , TypeEnv(..)
  , subst
  , subst'
  , putCtx
  , putTypeCtx
  , fv
  , quantify
  , withGlobalCtx
  , withGlobalTypeCtx
  , runTypecheckM
  ) where

import           AST                            ( ConMeta(..)
                                                , Expr(..)
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
import           Data.Functor                   ( ($>) )
import           Data.List                      ( intercalate )
import qualified Data.List.NonEmpty            as NE
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
                                                , liftEither
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
                                                , forAll
                                                , property
                                                )
import qualified Hedgehog
import qualified Hedgehog.Gen                  as G
import qualified Hedgehog.Range                as R

import           Data.Data                      ( Data )
import           Type.Primitive                 ( listConsMeta
                                                , listNilMeta
                                                )
import           Type.Reflection                ( Typeable )

-- Bidirectional typechecker
-- Following:
-- Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism

-- Primitive types
string :: Type
string = TCon (prim "String") []

int :: Type
int = TCon (prim "Int") []

char :: Type
char = TCon (prim "Char") []

bool :: Type
bool = TCon (prim "Bool") []

unit :: Type
unit = TCon (prim "Unit") []

list :: Type -> Type
list a = TCon (prim "List") [a]

io :: Type -> Type
io a = TCon (prim "IO") [a]

primTypeCtx :: TypeCtx
primTypeCtx = map
  (\n -> (prim n, ()))
  [ "String"
  , "Int"
  , "Char"
  , "Bool"
  , "Unit"
  , "List"
  , "IO"
  , "Tuple2"
  , "Tuple3"
  , "Tuple4"
  , "Tuple5"
  , "Tuple6"
  , "Tuple7"
  , "Tuple8"
  ]

-- Primitive constructors
-- TODO: move all primitive stuff to Type.Primitive

primitiveConstructors :: Ctx
primitiveConstructors =
  [ V (Free (prim "Unit"))  unit
  , V (Free (prim "True"))  bool
  , V (Free (prim "False")) bool
  , V (Free (prim "[]")) (Forall (U 0 "a") (list (UType (U 0 "a"))))
  , V
    (Free (prim "::"))
    (Forall
      (U 0 "a")
      (Fn (UType (U 0 "a"))
          (Fn (list (UType (U 0 "a"))) (list (UType (U 0 "a"))))
      )
    )
  , V (Free (prim "Tuple2")) (mkTupleCon 2 (prim "Tuple2"))
  , V (Free (prim "Tuple3")) (mkTupleCon 3 (prim "Tuple3"))
  , V (Free (prim "Tuple4")) (mkTupleCon 4 (prim "Tuple4"))
  , V (Free (prim "Tuple5")) (mkTupleCon 5 (prim "Tuple5"))
  , V (Free (prim "Tuple6")) (mkTupleCon 6 (prim "Tuple6"))
  , V (Free (prim "Tuple7")) (mkTupleCon 7 (prim "Tuple7"))
  , V (Free (prim "Tuple8")) (mkTupleCon 8 (prim "Tuple8"))
  , V
    (Free (prim "MkIO"))
    (Forall (U 0 "a")
            (Fn (Fn (Fn (UType (U 0 "a")) unit) unit) (io (UType (U 0 "a"))))
    )
  ]

-- Primitive functions
-- Note: the type of unconsChar is weird because we want to implement it
-- without any knowledge of the runtime structure of complex types like tuples
-- or Maybe, since that may change in the future.
primitiveFns :: Ctx
primitiveFns =
  [ V (Free (prim "appendString")) (Fn string (Fn string string))
  , V (Free (prim "$chars"))       (Fn string (list char))
  , V (Free (prim "$consChar"))    (Fn char (Fn string string))
  -- unconsChar : String -> a -> (Char -> String -> a) -> a
  , let a = U 0 "a"
    in
      V
        (Free (prim "$unconsChar"))

        (Forall
          a
          (Fn string
              (Fn (UType a) (Fn (Fn char (Fn string (UType a))) (UType a)))
          )
        )
  , V (Free (prim "+"))         (Fn int (Fn int int))
  , V (Free (prim "-"))         (Fn int (Fn int int))
  , V (Free (prim "*"))         (Fn int (Fn int int))
  , V (Free (prim "$showInt"))  (Fn int string)
  , V (Free (prim "$showChar")) (Fn char string)
  , V (Free (prim "$eqInt"))    (Fn int (Fn int bool))
  , V (Free (prim "$eqChar"))   (Fn char (Fn char bool))
  -- readInt : String -> a -> (Int -> a) -> a
  , let a = U 0 "a"
    in  V
          (Free (prim "$readInt"))

          (Forall a (Fn string (Fn (UType a) (Fn (Fn int (UType a)) (UType a))))
          )
  ]

primCtx :: Ctx
primCtx = primitiveConstructors <> primitiveFns

mkTupleCon :: Int -> Name -> Type
mkTupleCon len tcon =
  let us = map (uncurry U) $ take len $ zip
        [0 ..]
        (map (fromString . (: [])) ['a' ..])
  in  foldr Forall (foldr (Fn . UType) (TCon tcon (map UType us)) us) us

-- In the future we will support syntax to declare the types of foreign calls,
-- like this:
--     foreign putStrLn : String -> IO ()
--     foreign getLine : IO String
--     foreign bindIO : IO a -> (a -> IO b) -> IO b
-- Until then, we hard-code the types here.
-- If you add a new fcall here, you also need to add its implementation in
-- LC.Execute.
fcallInfo :: [(String, Type)]
fcallInfo =
  -- name        type
  [ ("putStrLn", Fn string unit)
  , ("putStr"  , Fn string unit)
  , ("getLine" , string)
  -- , ("pureIO", Forall (U 0 "a") (Fn (UType (U 0 "a")) (io (UType (U 0 "a")))))
  -- , ( "bindIO"
  --   , Forall (U 0 "a") $ Forall (U 1 "b") $ Fn (io (UType (U 0 "a"))) $ Fn
  --     (Fn (UType (U 0 "a")) (io (UType (U 1 "b"))))
  --     (io (UType (U 1 "b")))
  --   )
  ]

-- | Types
data Type =
  -- Function type
    Fn Type Type
  -- Universal quantifier
  | Forall U Type
  -- Existential variable
  | EType E
  -- Universal variable
  | UType U
  -- Type constructor (saturated)
  -- TODO: not always saturated! see subtyping for TApps
  | TCon Name [Type]
  -- Record type
  -- TODO: record typing rules
  | TRecord [(String, Type)]
  -- Type application
  -- unchecked invariant: always in spine form (i.e., head is never a TApp)
  | TApp Type [Type]
  deriving (Eq, Show, Typeable, Data)

data DebugPrintCtx = Neutral | AppL | AppR | ArrL | ArrR
instance Debug Type where
  debug = debug' AppR
   where
    debug' _ (EType e) = debug e
    debug' _ (UType u) = debug u
    debug' c (Fn a b ) = case c of
      Neutral -> debug' ArrL a <+> "->" <+> debug' ArrR b
      ArrR    -> debug' Neutral (Fn a b)
      _       -> "(" <> debug' Neutral (Fn a b) <> ")"
    debug' c (Forall v t) = case c of
      Neutral -> "∀" <> debug v <> "." <+> "(" <> debug' Neutral t <> ")"
      _       -> "(" <> debug' Neutral (Forall v t) <> ")"
    debug' _ (TCon d []  ) = debug d
    debug' c (TCon d args) = case c of
      Neutral -> debug d <+> sepBy " " (map debug args)
      AppL    -> "(" <> debug' Neutral (TCon d args) <> ")"
      AppR    -> "(" <> debug' Neutral (TCon d args) <> ")"
      _       -> debug' Neutral (TCon d args)
    debug' _ (TRecord fields) = "{" <+> sepBy ", " (map go fields) <+> "}"
      where go (name, ty) = name <+> ":" <+> debug' Neutral ty
    debug' c (TApp ty args) = case c of
      Neutral -> debug' AppL ty <+> sepBy " " (map (debug' AppR) args)
      AppR    -> "(" <> debug' Neutral (TApp ty args) <> ")"
      _       -> debug' Neutral (TApp ty args)

-- | Variables
-- Universal type variable
-- Contains a name hint
data U = U Int Name
  deriving (Show, Typeable, Data)

instance Eq U where
  (U i _) == (U j _) = i == j

instance Debug U where
  debug (U n v) = debug v <> show n

-- Existential type variable
newtype E = E Int
  deriving (Eq, Show, Typeable, Data)

instance Debug E where
  debug (E e) = "e" <> show e

-- Free or bound variable
-- Guaranteed to be unique.
-- Contains a name hint for conversion back to source.
data V = Free Name
       | Bound Int -- Not currently used, but should be for lambda bindings
  deriving (Eq, Show, Typeable, Data)

instance Debug V where
  debug (Free  n) = debug n
  debug (Bound i) = "v" <> show i

type Exp = Expr V Type


type Pattern = Pat V

-- | Contexts

-- | A mapping from constructor names to their tag, arity and type name.
type CtorInfo = [(Name, ConMeta)]

-- | A mapping of in-scope types to their kinds
-- We don't yet have kinds, so we just store () instead for the moment.
type TypeCtx = [(Name, ())]

-- | A local context, holding top level definitions and local variables
type Ctx = [CtxElem]

data CtxElem =
  -- Bound variable
    V V Type
  -- Universal type variable
  | UVar U
  -- Existential type variable
  | EVar E
  -- Solved existential type variable (to a monotype)
  | ESolved E Type
  -- Existential variable marker
  | Marker E
  deriving (Eq, Show)

instance Debug CtxElem where
  debug (V v t       ) = debug v <+> ":" <+> debug t
  debug (UVar u      ) = debug u
  debug (EVar e      ) = debug e
  debug (ESolved e ty) = debug e <+> "=" <+> debug ty
  debug (Marker e    ) = "'" <> debug e

debugCtx :: Ctx -> String
debugCtx ctx = "[" <> sepBy ", " (map debug ctx) <> "]"


-- | The free (existential) variables of the type
fv :: Type -> [E]
fv = \case
  Fn     a b     -> fv a <> fv b
  Forall _ a     -> fv a
  EType e        -> [e]
  UType _        -> []
  TCon _ as      -> concatMap fv as
  TRecord fields -> concatMap (fv . snd) fields
  TApp f as      -> fv f <> concatMap fv as

-- | Quantify the given existential variables by replacing them with forall-bound universal
-- variables.
quantify :: [E] -> Type -> TypeM Type
quantify vars t = do
  -- Generate a fresh UVar for each E
  uMap <- mapM (\e -> (e, ) <$> newU "") vars
  -- Construct a context mapping each E to its replacement UType
  let ctx = map (\(e, u) -> ESolved e (UType u)) uMap
  -- Apply the substitution to the type
  let t'  = subst' ctx t
  -- Wrap the result in foralls to bind each UType
  pure $ foldr (Forall . snd) t' uMap

-- | Apply the current context, as a substitution, to a type
subst :: Type -> TypeM Type
subst ty = subst' <$> getCtx <*> pure ty

subst' :: Ctx -> Type -> Type
subst' ctx = \case
  UType u        -> UType u
  Fn     a b     -> Fn (subst' ctx a) (subst' ctx b)
  Forall u a     -> Forall u (subst' ctx a)
  TCon   c as    -> TCon c (map (subst' ctx) as)
  TRecord fields -> TRecord $ mapSnd (subst' ctx) fields
  -- If the substitution yields a nested TCon or TApp, flatten it back to spine
  -- form.
  TApp f as ->
    let as' = map (subst' ctx) as
    in  case subst' ctx f of
          TCon c args -> TCon c (args <> as')
          TApp g cs   -> TApp g (cs <> as')
          f'          -> TApp f' as'
  EType e -> substE ctx
   where
    substE :: Ctx -> Type
    substE = \case
      []                              -> EType e
      (ESolved e' t : ctx') | e == e' -> subst' ctx' t
      (EVar e' : _) | e == e'         -> EType e
      (_ : c)                         -> substE c

-- | Substitute an existential variable for a universal variable
substEForU :: E -> U -> Type -> Type
substEForU e u = go
 where
  go = \case
    Fn a b -> Fn (go a) (go b)
    Forall u' a | u == u'   -> Forall u' a
                | otherwise -> Forall u' (go a)
    EType e' -> EType e'
    UType u' | u == u'   -> EType e
             | otherwise -> UType u'
    TCon c as      -> TCon c (map go as)
    TRecord fields -> TRecord (mapSnd go fields)
    TApp f as      -> TApp (go f) (map go as)

-- | The bound variables in a context
domV :: Ctx -> [V]
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
lookupV :: V -> TypeM Type
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
  case lookup name (tctx <> globalCtx) of
    Just _  -> pure ()
    Nothing -> throwError (UnknownType name)

-- Context construction

-- | Save the current state, run an action that returns a result,
-- then restore the original state
call :: TypeM a -> TypeM a
call m = do
  st     <- lift $ lift get
  result <- m
  lift $ lift $ put st
  pure result

-- | Extend the current context with a universal variable
extendU :: U -> TypeM ()
extendU u = do
  ctx <- getCtx
  if u `elem` domU ctx then todoError "extendU failed" else pushCtx (UVar u)

-- | Extend the current context with a bound variable
extendV :: V -> Type -> TypeM ()
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
      pushCtx (ESolved e t)

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

-- Check if a type is well-formed
wellFormedType :: Type -> TypeM ()
wellFormedType ty = do
  trace' ["wellFormedType", debug ty] $ case ty of
    Fn     a b -> wellFormedType a >> wellFormedType b
    Forall u t -> do
      call $ do
        void $ extendU u
        wellFormedType t
    UType u -> lookupU u
    EType e -> do
      lookupE e `catchError` const (void (lookupSolved e))
    -- TODO: check that c has the correct kind (i.e. for all its args)
    TCon c _as     -> lookupType c
    TRecord fields -> mapM_ (\(n, t) -> (n, ) <$> wellFormedType t) fields
    TApp f b       -> wellFormedType f >> mapM_ wellFormedType b

-- Typechecking monad
data TypeEnv = TypeEnv
  { envCtx     :: Ctx    -- The global type context
  , envTypeCtx :: TypeCtx -- Global type info (primitive types)
  , envDepth   :: Int  -- The recursion depth, used for debugging
  , envDebug   :: Bool -- Whether to output debug messages
  }
  deriving (Eq, Show)

defaultTypeEnv :: TypeEnv
defaultTypeEnv =
  let debugOn =
        case unsafePerformIO (fmap (== "true") <$> lookupEnv "KITE_DEBUG") of
          Just True -> True
          _         -> False
  in  TypeEnv { envCtx     = primCtx
              , envTypeCtx = primTypeCtx
              , envDepth   = 0
              , envDebug   = debugOn
              }

-- This type is used outside of this module, to raise errors and set the global context.
-- It also holds a counter for fresh variable names.
type TypecheckM = ReaderT TypeEnv (ExceptT LocatedError (State Int))

runTypecheckM :: TypeEnv -> TypecheckM a -> Either LocatedError a
runTypecheckM env m = evalState (runExceptT (runReaderT m env)) 0

-- This type is used only inside this module. It is like TypecheckM but it can also manipulate the
-- local context.
type TypeM = ReaderT TypeEnv (ExceptT LocatedError (State TypeState))

-- | Convert a TypeM action into a TypecheckM action.
-- We do this by supplying a default state value, but threading through the variable counter which
-- is present in TypecheckM. This ensures that we can generate fresh variables in both TypeM and
-- TypecheckM and they won't clash.
runTypeM :: TypeM a -> TypecheckM a
runTypeM m = do
  c <- lift $ lift get
  let defaultState =
        TypeState { varCounter = c, context = mempty, typeContext = mempty }
  env <- ask
  let m'            = runReaderT m env
  let m''           = runExceptT m'
  let (m''', state) = runState m'' defaultState
  lift $ lift $ put (varCounter state)
  liftEither m'''

-- Functions for modifying the global type context
-- These are used by other modules, which control the global context.
-- The local context should be managed only by this module.
withGlobalCtx :: MonadReader TypeEnv m => (Ctx -> Ctx) -> m a -> m a
withGlobalCtx f = local (\e -> e { envCtx = f (envCtx e) })

withGlobalTypeCtx
  :: MonadReader TypeEnv m => (TypeCtx -> TypeCtx) -> m a -> m a
withGlobalTypeCtx f = local (\e -> e { envTypeCtx = f (envTypeCtx e) })

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
  i <- lift (lift (gets varCounter))
  lift $ lift $ modify' (\st -> st { varCounter = i + 1 })
  pure i

newE :: TypeM E
newE = E <$> newInt

newU :: Name -> TypeM U
newU hint = U <$> newInt <*> pure hint

-- Get the current local context
getCtx :: TypeM Ctx
getCtx = lift (lift (gets context))

-- Replace the current local context
putCtx :: Ctx -> TypeM ()
putCtx ctx = lift (lift (modify' (\st -> st { context = ctx })))

-- Push a new element on to the local context
pushCtx :: CtxElem -> TypeM ()
pushCtx e = do
  ctx <- getCtx
  putCtx (e : ctx)

-- | Get the current type context
getTypeCtx :: TypeM TypeCtx
getTypeCtx = lift $ lift $ gets typeContext

-- | Replace the current type context
putTypeCtx :: TypeCtx -> TypeM ()
putTypeCtx tctx = lift $ lift $ modify' $ \st -> st { typeContext = tctx }


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
    (UType a, UType a') | a == a' -> lookupU a
    (EType a, EType a') | a == a' -> lookupE a
    (Fn a1 a2, Fn b1 b2)          -> do
      subtype' b1 a1
      a2' <- subst a2
      b2' <- subst b2
      subtype' a2' b2'
    (a, Forall u b) -> do
      void $ extendU u
      subtype' a b
      dropAfter (UVar u)
    (Forall u a, b) -> do
      alpha <- newE
      void $ extendMarker alpha
      void $ extendE alpha
      subtype' (substEForU alpha u a) b
      dropAfter (Marker alpha)
    (EType e, a) | e `elem` fv a -> throwError $ OccursCheck e a
                 | otherwise     -> instantiateL e a
    (a, EType e) | e `elem` fv a -> throwError $ OccursCheck e a
                 | otherwise     -> instantiateR e a
    (TCon v as, TCon v' bs) | v == v' ->
      forM_ (zip as bs) (bimapM subst subst >=> uncurry subtype')
    (TCon c as, TApp v bs) -> do
      subtype' (TCon c []) v
      mapM_ (uncurry subtype') (zip as bs)
    (TApp v as, TCon c bs) -> do
      let a = foldApp v as
      let b = foldApp (TCon c []) bs
      subtype' a b
    (TApp v as, TApp u bs) -> do
      subtype' v u
      mapM_ (uncurry subtype') (zip as bs)
    (TRecord as, TRecord bs) -> do
      -- For any records A, B
      -- A < B if A and B have the same set of labels and every field fB in B
      -- has a corresponding field fA in A such that fA < fB
      forM_ bs $ \(label, bTy) -> case lookup label as of
        Nothing  -> throwError $ SubtypingFailure (TRecord as) (TRecord bs)
        Just aTy -> do
          aTy' <- subst aTy
          bTy' <- subst bTy
          subtype' aTy' bTy'
      forM_ as $ \(label, _) -> case lookup label bs of
        Just _  -> pure ()
        Nothing -> throwError $ SubtypingFailure (TRecord as) (TRecord bs)

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
    EType f ->
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
          putCtx $ l <> [ESolved f (EType e)] <> r
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
                       putCtx $ l <> [ESolved e (EType f)] <> r
                     )
    Fn a b -> do
      let (l, r) = splitAt (EVar e) ctx
      a1 <- newE
      a2 <- newE
      putCtx
        (l <> [ESolved e (Fn (EType a1) (EType a2)), EVar a1, EVar a2] <> r)
      instantiate (flipDir dir) a1 a
      b' <- subst b
      instantiate (flipDir dir) a2 b'
    Forall u a -> case dir of
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
      es <- mapM (const newE) args
      -- insert them into the context before e, along with a solution e = TCon c es
      putCtx $ l <> [ESolved e (TCon c (map EType es))] <> map EVar es <> r
      -- instantiate each arg to its corresponding e
      mapM_ (uncurry $ instantiate (flipDir dir)) (zip es args)

    -- We do the same for type applications
    -- TODO: does this work if f contains existential vars?
    TApp f args -> do
      let (l, r) = splitAt (EVar e) ctx
      -- generate new Es for each arg
      es <- mapM (const newE) args
      -- insert them into the context before e, along with a solution e = f es
      putCtx $ l <> [ESolved e (TApp f (map EType es))] <> map EVar es <> r
      -- instantiate each arg to its corresponding e
      mapM_ (uncurry $ instantiate (flipDir dir)) (zip es args)
    a -> do
      let (l, r) = splitAt (EVar e) ctx
      call $ do
        putCtx r
        wellFormedType a
      putCtx $ l <> [ESolved e a] <> r
 where
  flipDir :: InstDir -> InstDir
  flipDir L = R
  flipDir R = L

-- Typing
{-# ANN check ("HLint: ignore Reduce duplication" :: String) #-}
check :: Exp -> Type -> TypeM ()
check expr ty = do
  trace' ["check", debug expr, ":", debug ty] $ case (expr, ty) of
    (Abs []       e, a     ) -> check e a
    (Abs (x : xs) e, Fn a b) -> do
      void $ extendV x a
      _ <- check (Abs xs e) b
      dropAfter (V x a)
    (e, Forall u a) -> do
      extendU u
      check e a
      dropAfter (UVar u)
    (Hole n        , a) -> throwError $ CannotCheckHole (Hole n) a
    (Let binds body, _) -> do
      -- generate a dummy existential that we'll use to cut the context
      alpha <- newE
      void $ extendMarker alpha

      -- if the binding is annotated, check it against its annotation
      -- otherwise infer a type for it
      -- then add the type to the context
      forM_ binds $ \(x, e, maybeType) -> do
        t <- case maybeType of
          Just t  -> check e t $> t
          Nothing -> infer e
        extendV x t

      -- check the type of the body
      check body ty

      -- drop all these variables off the context
      dropAfter (Marker alpha)
    (FCall name args, _) -> do
      case lookup name fcallInfo of
        Just fCallTy -> do
          resultTy  <- foldM inferApp fCallTy args
          resultTy' <- subst resultTy
          subtype resultTy' ty
        Nothing -> throwError $ UnknownFCall name
    (MCase []                  , _     ) -> throwError (EmptyCase expr)
    (MCase alts@((pats, _) : _), Fn a b) -> do
      -- Split off as many args as there are patterns in the first alt
      let (argTys, exprTy) =
            let (as, b') = unfoldFn (Fn a b)
            in  (take (length pats) as, foldFn (drop (length pats) as) b')
      mapM_ (checkMCaseAlt argTys exprTy) alts
    (Case scrut alts, _) -> do
      scrutTy <- infer scrut
      mapM_ (checkCaseAlt ty scrutTy) alts
    (e, b) -> do
      a  <- infer e >>= subst
      b' <- subst b
      subtype a b'

checkMCaseAlt :: [Type] -> Type -> ([Pattern], Exp) -> TypeM ()
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
  check rhs rhsTy

  -- drop all context elements after and including the marker
  dropAfter (Marker alpha)

-- Like infer but applies the resulting substitution to the type and returns
-- just the type.
-- Useful for tests.
infer' :: Exp -> TypeM Type
infer' e = trace' ["infer'", debug e] $ do
  ty <- infer e
  subst ty

-- TODO: flip this around so it's Ctx -> Exp -> TypeM (Ctx, Typ)
-- This fits with foldM and mapAccumLM
infer :: Exp -> TypeM Type
infer expr_ = do
  trace' ["infer", debug expr_] $ case expr_ of
    Var x   -> lookupV x
    Ann e a -> do
      void $ wellFormedType a
      check e a
      pure a
    App e1 e2 -> do
      a <- infer e1 >>= subst
      inferApp a e2
    Abs []       e -> infer e
    Abs (x : xs) e -> do
      alpha <- newE
      beta  <- newE
      extendE alpha >> extendE beta >> extendV x (EType alpha)
      _ <- check (Abs xs e) (EType beta)
      dropAfter (V x (EType alpha))
      pure $ Fn (EType alpha) (EType beta)
    Hole n                     -> throwError $ CannotInferHole (Hole n)
    Con  x                     -> lookupV x
    c@(  Case _ [])            -> throwError $ EmptyCase c
    Case scrut    (alt : alts) -> do
      scrutTy <- infer scrut
      altTy   <- inferCaseAlt scrutTy alt
      -- altTy might have foralls in it, for example if alt is []
      -- the other alts may have valid concrete types like [Int],
      -- and if we blindly check that they are subtypes of [] then we'll raise a
      -- type error.
      -- Instead, we strip off all the foralls, generating existentials for each
      -- one, and then check the alts.
      altTy'  <- existentialiseOuterForalls altTy
      mapM_ (checkCaseAlt altTy' scrutTy) alts
      pure altTy
    c@(MCase [])                -> throwError $ EmptyCase c
    MCase ((pats, expr) : alts) -> do
      -- The type of an mcase will be a function type taking as many arguments as
      -- there are patterns. Each alt should have the same number of patterns.
      -- Taking the first alt, we infer a type for each pattern and a type for the
      -- RHS expr. We then check the remaining alts against this. This yields a
      -- function type which we return. It may have existential variables that
      -- need to be resolved later on.

      -- First, we infer a type for each pattern in the first alt
      patTys <- mapM inferPattern pats
      -- Next, infer a type for the RHS
      exprTy <- infer expr
      -- Now check the remaining alts using this information
      mapM_ (checkMCaseAlt patTys exprTy) alts
      -- Now construct a result type and return it
      pure $ foldFn patTys exprTy
    Let binds body -> do
      -- generate a dummy existential that we'll use to cut the context
      alpha <- newE
      void $ extendMarker alpha

      -- if the binding is annotated, check it against its annotation
      -- otherwise infer a type for it
      -- then add the type to the context
      forM_ binds $ \(x, e, maybeType) -> do
        t <- case maybeType of
          Just t  -> check e t $> t
          Nothing -> infer e
        extendV x t

      -- infer the type of the body
      ty  <- infer body

      -- drop all these variables off the context
      ty' <- subst ty
      dropAfter (Marker alpha)
      pure ty'
    StringInterp prefix comps -> do
      -- Construct a nested application of appendString and infer the type of that
      let append = Var (Free (prim "appendString"))
          expr   = foldl
            (\acc (c, s) -> App (App append acc) (App (App append c) s))
            (StringLit prefix)
            (mapSnd StringLit (NE.toList comps))
      infer expr
    StringLit _   -> pure string
    CharLit   _   -> pure char
    IntLit    _   -> pure int
    BoolLit   _   -> pure bool
    UnitLit       -> pure unit
    ListLit elems -> do
      -- Construct a nested application of (::) and [] and infer the type of that
      let expr = foldr (App . App (Con (Free (prim "::"))))
                       (Con (Free (prim "[]")))
                       elems
      infer expr
    TupleLit elems -> do
      -- Construct an application of TupleN and infer the type of that
      let n    = length elems
          con  = Con $ Free $ prim $ fromString $ "Tuple" <> show n
          expr = foldl App con elems
      infer expr
    Record fields -> do
      -- To infer the type of a record, we must be able to infer types for all its
      -- fields
      fTypes <- forM
        fields
        (\(name, expr) -> do
          ty <- infer expr >>= subst
          pure (name, ty)
        )
      pure $ TRecord fTypes
    Project record fieldName -> do
      -- To infer the type of a record projection, we must know the type of the
      -- record.
      recordTy <- infer record
      -- Check if the record contains this field
      subst recordTy >>= \case
        TRecord fields -> case lookup fieldName fields of
          Just fieldTy -> pure fieldTy
          Nothing      -> throwError $ RecordDoesNotHaveField recordTy fieldName
        _ -> throwError $ NotARecordType recordTy
    e@(FCall _name _args) -> throwError $ CannotInferFCall e

-- Strip off all the outer foralls from a type, replacing them with
-- existentials.
-- [] forall a b c. t ==> [e1 e2 e3] [e3/c][e2/b][e1/a]t
existentialiseOuterForalls :: Type -> TypeM Type
existentialiseOuterForalls (Forall u a) = do
  alpha <- newE
  extendE alpha
  existentialiseOuterForalls (substEForU alpha u a)
existentialiseOuterForalls t = pure t

checkCaseAlt :: Type -> Type -> (Pattern, Exp) -> TypeM ()
checkCaseAlt expectedAltTy scrutTy (pat, expr) = do
  trace'
      [ "checkCaseAlt"
      , debug scrutTy
      , debug expectedAltTy
      , debug pat
      , debug expr
      ]
    $ do
        checkPattern pat scrutTy
        check expr expectedAltTy

-- TODO: probably better to infer the alts first, since they often constrain the
-- scrut type, and then we don't need to infer it.
inferCaseAlt :: Type -> (Pattern, Exp) -> TypeM Type
inferCaseAlt scrutTy (pat, expr) = do
  trace' ["inferCaseAlt", debug scrutTy, debug pat, debug expr] $ do
    checkPattern pat scrutTy
    infer expr

{-# ANN inferPattern ("HLint: ignore Reduce duplication" :: String) #-}
inferPattern :: Pattern -> TypeM Type
inferPattern pat = trace' ["inferPattern", debug pat] $ case pat of
  IntPat  _                 -> pure int
  CharPat _                 -> pure char
  BoolPat _                 -> pure bool
  UnitPat                   -> pure unit
  StringPat _               -> pure string
  ConsPat con _meta subpats -> do
      -- Lookup the type of the constructor
    conTy <- lookupV con
    case second unfoldFn (unfoldForall conTy) of
      (us, (argTys, tcon@TCon{})) -> do
        -- Create new existentials for each universal in the forall
        -- Add them to the context
        eSub <- mapM (\u -> (, u) <$> newE) us
        mapM_ (extendE . fst) eSub

        -- Substitute them into the argtys and the tcon
        let argTys' = map
              (\t -> foldl (\t' (e, u) -> substEForU e u t') t eSub)
              argTys
            tcon' = foldl (\t' (e, u) -> substEForU e u t') tcon eSub

        -- Check each subpattern against the corresponding argty
        mapM_ (uncurry checkPattern) (zip subpats argTys')

        -- Return the constructor type
        pure tcon'
      (_, (_, t)) -> throwError $ ExpectedConstructorType t
  VarPat x -> do
    alpha <- newE
    extendE alpha >> extendV x (EType alpha)
    pure (EType alpha)
  WildPat -> do
    alpha <- newE
    extendE alpha
    pure (EType alpha)
  TuplePat subpats ->
    let con = case subpats of
          []                       -> error "Type.inferPattern: empty tuple"
          [_] -> error "Type.inferPattern: single-element tuple"
          [_, _]                   -> Free (prim "Tuple2")
          [_, _, _]                -> Free (prim "Tuple3")
          [_, _, _, _]             -> Free (prim "Tuple4")
          [_, _, _, _, _]          -> Free (prim "Tuple5")
          [_, _, _, _, _, _]       -> Free (prim "Tuple6")
          [_, _, _, _, _, _, _]    -> Free (prim "Tuple7")
          [_, _, _, _, _, _, _, _] -> Free (prim "Tuple8")
          _ ->
            error
              $  "Type.inferPattern: cannot (yet) handle tuples of length > 8: "
              <> show subpats
    in  inferPattern (ConsPat con Nothing subpats)
  ListPat [] -> inferPattern (ConsPat (Free (prim "[]")) (Just listNilMeta) [])
  ListPat subpats -> inferPattern
    (foldr (\s acc -> ConsPat (Free (prim "::")) (Just listConsMeta) [s, acc])
           (ConsPat (Free (prim "[]")) (Just listNilMeta) [])
           subpats
    )

checkPattern :: Pattern -> Type -> TypeM ()
checkPattern pat ty = do
  trace' ["checkPattern", debug pat, ":", debug ty] $ case pat of
    WildPat  -> pure ()
    VarPat x -> do
      ctx <- getCtx
      if x `elem` domV ctx
        then throwError (DuplicateVariable x)
        else void $ extendV x ty
    IntPat  _                 -> subtype ty int
    CharPat _                 -> subtype ty char
    BoolPat _                 -> subtype ty bool
    UnitPat                   -> subtype ty unit
    StringPat _               -> subtype ty string
    ConsPat con _meta subpats -> do
      constructorType <- lookupV con
      case second unfoldFn (unfoldForall constructorType) of
        (us, (argTys, tcon@TCon{})) -> do
          -- Create new existentials for each universal in the forall
          eSub <- mapM (\u -> (, u) <$> newE) us

          -- Add new existentials to the context
          mapM_ (extendE . fst) eSub

          -- Substitute them into the argtys and the tcon
          let
            argTys' =
              map (\t -> foldl (\t' (e, u) -> substEForU e u t') t eSub) argTys
            tcon' = foldl (\t' (e, u) -> substEForU e u t') tcon eSub

          -- Check each subpattern against the corresponding argty
          mapM_ (uncurry checkPattern) (zip subpats argTys')

          -- Check this against the type we have been given
          tcon'' <- subst tcon'
          ty'    <- subst ty
          subtype tcon'' ty'
        (_, (_, t)) -> throwError $ ExpectedConstructorType t
    ListPat [] ->
      checkPattern (ConsPat (Free (prim "[]")) (Just listNilMeta) []) ty
    ListPat subpats -> checkPattern
      (foldr
        (\s acc -> ConsPat (Free (prim "::")) (Just listConsMeta) [s, acc])
        (ConsPat (Free (prim "[]")) (Just listNilMeta) [])
        subpats
      )
      ty
    TuplePat subpats ->
      let
        con = case subpats of
          []                       -> error "Type.checkPattern: empty tuple"
          [_] -> error "Type.checkPattern: single-element tuple"
          [_, _]                   -> Free (prim "Tuple2")
          [_, _, _]                -> Free (prim "Tuple3")
          [_, _, _, _]             -> Free (prim "Tuple4")
          [_, _, _, _, _]          -> Free (prim "Tuple5")
          [_, _, _, _, _, _]       -> Free (prim "Tuple6")
          [_, _, _, _, _, _, _]    -> Free (prim "Tuple7")
          [_, _, _, _, _, _, _, _] -> Free (prim "Tuple8")
          _ ->
            error
              $  "Type.checkPattern: cannot (yet) handle tuples of length > 8: "
              <> show subpats
      in  checkPattern (ConsPat con Nothing subpats) ty

-- | unfoldForall (Forall a (Forall b t)) == ([a, b], t)
unfoldForall :: Type -> ([U], Type)
unfoldForall (Forall u t) = let (us, t') = unfoldForall t in (u : us, t')
unfoldForall t            = ([], t)

-- | unfoldFn (Fn a (Fn b (Fn c d))) = ([a, b, c], d)
unfoldFn :: Type -> ([Type], Type)
unfoldFn (Fn a b) = let (ts, t) = unfoldFn b in (a : ts, t)
unfoldFn t        = ([], t)

-- | foldFn [a, b, c] d = (Fn a (Fn b (Fn c d)))
foldFn :: [Type] -> Type -> Type
foldFn []       t = t
foldFn [a     ] t = Fn a t
foldFn (a : as) t = Fn a (foldFn as t)

-- | foldApp A [b, c, d] = TApp (TApp (TApp A b) c) d
foldApp :: Type -> [Type] -> Type
foldApp = foldl (\f t -> TApp f [t])

-- Infer the type of the result when applying a function of type @ty@ to @e@
inferApp :: Type -> Exp -> TypeM Type
inferApp ty e = do
  trace' ["inferApp", debug ty, debug e] $ case ty of
    Forall u a -> do
      alpha <- newE
      extendE alpha
      inferApp (substEForU alpha u a) e
    EType alpha -> do
      a1 <- newE
      a2 <- newE
      extendE a2 >> extendE a1 >> extendSolved alpha (Fn (EType a1) (EType a2))
      check e (EType a1)
      pure $ EType a2
    Fn a b -> do
      check e a
      pure b
    _ -> throwError $ InfAppFailure ty e

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
           | UnknownVariable V
           | UnknownFCall String
           | EmptyCase Exp
           | ExpectedConstructorType Type
           | RecordDoesNotHaveField Type String
           | NotARecordType Type
           | CannotInferFCall Exp
           | TooManyPatterns
           | DuplicateVariable V
           | UnknownType Name
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

genType :: Gen Type
genType = G.recursive
  G.choice
  [EType <$> genE, UType <$> genU]
  [ G.subterm2 genType genType Fn
  , G.subtermM genType (\t -> Forall <$> genU <*> pure t)
  , TCon <$> genName <*> G.list (R.linear 0 3) genType
  ]

genCtxElem :: Gen CtxElem
genCtxElem = G.choice
  [ V <$> genV <*> genType
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

genV :: Gen V
genV = G.choice [Free <$> genName, Bound <$> G.int (R.linear 0 100)]

genModuleName :: Gen ModuleName
genModuleName = ModuleName <$> G.list (R.linear 1 3) genUpperString

genUpperString :: Gen String
genUpperString = do
  c  <- G.upper
  cs <- G.list (R.linear 0 10) G.alphaNum
  pure (c : cs)

genUpperRawName :: Gen RawName
genUpperRawName = Name <$> genUpperString

test :: Hedgehog.Group
test = $$(Hedgehog.discover)

prop_fv_commutes :: Property
prop_fv_commutes = property $ do
  a <- forAll genType
  b <- forAll genType
  fv (Fn a b) === (fv a <> fv b)

prop_subst_commutes :: Property
prop_subst_commutes = property $ do
  ctx <- forAll genCtx
  a   <- forAll genType
  b   <- forAll genType
  subst' ctx (Fn a b) === Fn (subst' ctx a) (subst' ctx b)

prop_substEForU_commutes :: Property
prop_substEForU_commutes = property $ do
  e <- forAll genE
  u <- forAll genU
  a <- forAll genType
  b <- forAll genType
  substEForU e u (Fn a b) === Fn (substEForU e u a) (substEForU e u b)

typecheckTest :: Ctx -> TypeM a -> Either LocatedError a
typecheckTest ctx =
  runTypecheckM defaultTypeEnv . withGlobalCtx (<> ctx) . runTypeM

prop_uval_infers_unit :: Property
prop_uval_infers_unit = property $ do
  ctx <- forAll genCtx
  let expr = Con (Free (prim "Unit"))
      ty   = TCon (prim "Unit") []
  typecheckTest ctx (infer expr) === Right ty

prop_checks_bad_unit_annotation :: Property
prop_checks_bad_unit_annotation = property $ do
  ctx <- forAll genCtx
  let expr = Con (Free (prim "Unit"))
      ty   = Fn unit unit
  typecheckTest ctx (check expr ty)
    === Left (LocatedError Nothing (SubtypingFailure unit (Fn unit unit)))

prop_infers_simple_app :: Property
prop_infers_simple_app = property $ do
  v <- forAll genV
  let uval = Con (Free (prim "Unit"))
  let expr = App (Ann (Abs [v] uval) (Fn unit unit)) uval
  typecheckTest mempty (infer expr) === Right unit

prop_infers_app_with_context :: Property
prop_infers_app_with_context = property $ do
  -- The context contains id : Unit -> Unit
  let uval = Con (Free (prim "Unit"))
      ctx  = [V (Free "id") (Fn unit unit)]
  -- The expression is id Unit
  let expr = App (Var (Free "id")) uval
  -- The inferred type should be Unit
  typecheckTest ctx (infer expr) === Right unit

prop_infers_polymorphic_app :: Property
prop_infers_polymorphic_app = property $ do
  -- The context contains id     : forall a. a -> a
  --                      idUnit : Unit -> Unit
  let uval = Con (Free (prim "Unit"))
      a    = U 0 "a"
      ctx =
        [ V (Free "id")     (Forall a (Fn (UType a) (UType a)))
        , V (Free "idUnit") (Fn unit unit)
        ]
  -- The expression is idUnit (id Unit)
  let expr = App (Var (Free "idUnit")) (App (Var (Free "id")) uval)
  -- The inferred type should be Unit
  typecheckTest ctx (infer expr) === Right unit

prop_infers_list_app :: Property
prop_infers_list_app = property $ do
  let a    = U 0 "a"
      ty   = Forall a (list (UType a))
      -- [] : forall a. List a
      ctx  = [V (Free "[]") ty]
      expr = Var (Free "[]")
  typecheckTest ctx (infer expr) === Right ty

prop_infers_bool_case :: Property
prop_infers_bool_case = property $ do
  let
      -- case True of
      --   True -> ()
      --   False -> ()
      expr1 = Case
        (Con (Free (prim "True")))
        [ (BoolPat True , Con (Free (prim "Unit")))
        , (BoolPat False, Con (Free (prim "Unit")))
        ]
      -- case True of
      --   True -> True
      --   False -> ()
      expr2 = Case
        (Con (Free (prim "True")))
        [ (BoolPat True , Con (Free (prim "True")))
        , (BoolPat False, Con (Free (prim "Unit")))
        ]
  typecheckTest mempty (infer expr1) === Right unit
  typecheckTest mempty (infer expr2)
    === Left (LocatedError Nothing (SubtypingFailure unit bool))

prop_checks_higher_kinded_application :: Property
prop_checks_higher_kinded_application = property $ do
  -- (\x -> x) : f a -> f a should check
  let
    f     = U 1 "f"
    a     = U 2 "a"
    expr1 = Abs [Bound 0] (Var (Bound 0))
    type1 = Forall
      f
      (Forall a (Fn (TApp (UType f) [UType a]) (TApp (UType f) [UType a])))
  typecheckTest mempty (check expr1 type1) === Right ()
  -- ((\x -> x) : f a -> f a) [1] : [Int] should check
  -- ((\x -> x) : f a -> f a) [1]         should infer [Int]
  let expr2 = App
        (Ann expr1 type1)
        (App (App (Var (Free (prim "::"))) (IntLit 1)) (Var (Free (prim "[]"))))
      type2 = list int
  typecheckTest mempty (check expr2 type2) === Right ()
  typecheckTest mempty (infer' expr2) === Right (list int)
  -- ((\x -> x) : f a -> f a) True : [Int] should fail to check
  -- ((\x -> x) : f a -> f a) True         should infer some unknown unification variables
  let expr3 = App (Ann expr1 type1) (Var (Free (prim "True")))
      type3 = list int
  typecheckTest mempty (check expr3 type3) === Left
    (LocatedError
      Nothing
      (SubtypingFailure (TCon (prim "Bool") [EType (E 1)]) (list int))
    )
  typecheckTest mempty (infer expr3)
    === Right (TApp (EType (E 0)) [EType (E 1)])
