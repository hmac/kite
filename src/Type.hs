{-# LANGUAGE TemplateHaskell #-}
module Type
  ( tests
  )
where

import           Prelude                 hiding ( splitAt )

import           Data.Maybe                     ( mapMaybe
                                                , listToMaybe
                                                )
import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( void
                                                , MonadPlus
                                                , mzero
                                                , guard
                                                )
import           Data.Functor                   ( ($>) )

import           Control.Monad.Trans.State.Strict
                                                ( State
                                                , evalState
                                                , get
                                                , put
                                                )
import           Control.Monad.Trans.Maybe      ( MaybeT(..)
                                                , runMaybeT
                                                )
import           Control.Monad.Trans.Class      ( lift )

import qualified Hedgehog.Gen                  as G
import qualified Hedgehog.Range                as R
import           Hedgehog                       ( Property
                                                , property
                                                , Gen
                                                , (===)
                                                , forAll
                                                )
import qualified Hedgehog

-- Util functions
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

-- Bidirectional typechecker
-- Following:
-- Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism

-- | Types
data Type =
  -- Unit type
    Unit
  -- Function type
  | Fn Type Type
  -- Universal quantifier
  | Forall U Type
  -- Existential variable
  | EType E
  -- Universal variable
  | UType U
  deriving (Eq, Show)

genType :: Gen Type
genType = G.recursive
  G.choice
  [pure Unit, EType <$> genE, UType <$> genU]
  [ G.subterm2 genType genType Fn
  , G.subtermM genType (\t -> Forall <$> genU <*> pure t)
  ]

-- | Variables
-- Universal type variable
newtype U = U Int
  deriving (Eq, Show)

genU :: Gen U
genU = U <$> G.int (R.linear 0 100)

-- Existential type variable
newtype E = E Int
  deriving (Eq, Show)

genE :: Gen E
genE = E <$> G.int (R.linear 0 100)

-- Bound variable
newtype V = V Int
  deriving (Eq, Show)

genV :: Gen V
genV = V <$> G.int (R.linear 0 100)

-- | Expressions
data Exp =
  -- Variable
    VarExp V
  -- Annotation
  | Ann Exp Type
  -- Application
  | App Exp Exp
  -- Lambda
  | Lam V Exp
  -- Unit
  | UVal
  deriving (Eq, Show)


-- | Contexts
type Ctx = [CtxElem]

genCtx :: Gen Ctx
genCtx = G.list (R.linear 1 10) genCtxElem

data CtxElem =
  -- Bound variable
    Var V Type
  -- Universal type variable
  | UVar U
  -- Existential type variable
  | EVar E
  -- Solved existential type variable (to a monotype)
  | ESolved E Type
  -- Existential variable marker
  | Marker E
  deriving (Eq, Show)

genCtxElem :: Gen CtxElem
genCtxElem = G.choice
  [ Var <$> genV <*> genType
  , UVar <$> genU
  , EVar <$> genE
  , ESolved <$> genE <*> genType
  , Marker <$> genE
  ]

-- | The free (existential) variables of the type
fv :: Type -> [E]
fv = \case
  Unit       -> []
  Fn     a b -> fv a <> fv b
  Forall _ a -> fv a
  EType e    -> [e]
  UType _    -> []

prop_fv_commutes :: Property
prop_fv_commutes = property $ do
  a <- forAll genType
  b <- forAll genType
  fv (Fn a b) === (fv a <> fv b)

-- | Apply a context, as a substitution, to a type
subst :: Ctx -> Type -> Type
subst ctx = \case
  UType u    -> UType u
  Unit       -> Unit
  Fn     a b -> Fn (subst ctx a) (subst ctx b)
  Forall u a -> Forall u (subst ctx a)
  EType e    -> substE ctx
   where
    substE :: Ctx -> Type
    substE = \case
      []                           -> EType e
      (ESolved e' t : _) | e == e' -> t
      (EVar e' : _) | e == e'      -> EType e
      (_ : c)                      -> substE c

prop_subst_commutes :: Property
prop_subst_commutes = property $ do
  ctx <- forAll genCtx
  a   <- forAll genType
  b   <- forAll genType
  subst ctx (Fn a b) === Fn (subst ctx a) (subst ctx b)

-- | Substitute an existential variable for a universal variable
substEForU :: E -> U -> Type -> Type
substEForU e u = go
 where
  go = \case
    Unit   -> Unit
    Fn a b -> Fn (go a) (go b)
    Forall u' a | u == u'   -> Forall u' a
                | otherwise -> Forall u' (go a)
    EType e' -> EType e'
    UType u' | u == u'   -> EType e
             | otherwise -> UType u'

prop_substEForU_commutes :: Property
prop_substEForU_commutes = property $ do
  e <- forAll genE
  u <- forAll genU
  a <- forAll genType
  b <- forAll genType
  substEForU e u (Fn a b) === Fn (substEForU e u a) (substEForU e u b)

-- | The bound variables in a context
domV :: Ctx -> [V]
domV = mapMaybe $ \case
  Var v _ -> Just v
  _       -> Nothing

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
lookupU :: U -> Ctx -> TypeM U
lookupU u = liftMaybe1 . firstJust $ \case
  (UVar u') | u' == u -> Just u
  _                   -> Nothing

-- | Lookup an existential variable in the context
lookupE :: E -> Ctx -> TypeM E
lookupE e = liftMaybe1 . firstJust $ \case
  (EVar e') | e' == e -> Just e
  _                   -> Nothing

lookupSolved :: E -> Ctx -> TypeM (E, Type)
lookupSolved e = liftMaybe1 . firstJust $ \case
  (ESolved e' t) | e' == e -> Just (e', t)
  _                        -> Nothing

lookupV :: V -> Ctx -> TypeM Type
lookupV v = liftMaybe1 . firstJust $ \case
  (Var v' t) | v' == v -> Just t
  _                    -> Nothing

-- Context construction

emptyCtx :: Ctx
emptyCtx = []

-- | Extend a context with a universal variable
extendU :: U -> Ctx -> TypeM Ctx
extendU u ctx | u `notElem` domU ctx = pure $ UVar u : ctx
              | otherwise            = liftMaybe Nothing

-- | Extend a context with a bound variable
extendV :: V -> Type -> Ctx -> TypeM Ctx
extendV v t ctx
  | v `notElem` domV ctx = wellFormedType ctx t >> pure (Var v t : ctx)
  | otherwise            = liftMaybe Nothing

-- | Extend a context with an existential variable
extendE :: E -> Ctx -> TypeM Ctx
extendE e ctx | e `notElem` domE ctx = pure $ EVar e : ctx
              | otherwise            = liftMaybe Nothing

-- | Extend a context with a solved existential variable
extendSolved :: E -> Type -> Ctx -> TypeM Ctx
extendSolved e t ctx
  | e `notElem` domE ctx = wellFormedType ctx t >> pure (ESolved e t : ctx)
  | otherwise            = liftMaybe Nothing

-- | Extend a context with an existential marker
extendMarker :: E -> Ctx -> TypeM Ctx
extendMarker e ctx
  | e `notElem` (domE ctx <> markers ctx) = pure $ Marker e : ctx
  | otherwise                             = liftMaybe Nothing

-- | Split a context at a given element, dropping that element
splitAt :: CtxElem -> Ctx -> (Ctx, Ctx)
splitAt e = \case
  []                   -> ([], [])
  (e' : ctx) | e' == e -> ([], ctx)
  (c : ctx)            -> let (l, r) = splitAt e ctx in (c : l, r)

-- Check if a type is well-formed
wellFormedType :: Ctx -> Type -> TypeM Type
wellFormedType ctx = \case
  Unit       -> pure Unit
  Fn     a b -> wellFormedType ctx a >> wellFormedType ctx b
  Forall u t -> do
    ctx' <- extendU u ctx
    _    <- wellFormedType ctx' t
    pure $ Forall u t
  UType u -> lookupU u ctx >> pure (UType u)
  EType e -> do
    void (lookupE e ctx) <|> void (lookupSolved e ctx)
    pure (EType e)

-- Typechecking monad
type TypeM = MaybeT (State Int)

runTypeM :: TypeM a -> Maybe a
runTypeM m = evalState (runMaybeT m) 0

newInt :: TypeM Int
newInt = do
  i <- lift get
  lift $ put (i + 1)
  pure i

newE :: TypeM E
newE = E <$> newInt

-- | Lift a 'Maybe' value into the 'MaybeT' monad transformer.
liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero return

liftMaybe1 :: MonadPlus m => (a -> Maybe b) -> a -> m b
liftMaybe1 m x = liftMaybe (m x)

-- Contexts with holes
-- TODO

-- Subtyping
-- | Under this input context, the first type is a subtype of the second type,
-- with the given output context
subtype :: Ctx -> Type -> Type -> TypeM Ctx
subtype ctx typeA typeB = case (typeA, typeB) of
  (UType a, UType a') | a == a' -> lookupU a ctx $> ctx
  (Unit, Unit)                  -> pure ctx
  (EType a, EType a') | a == a' -> lookupE a ctx $> ctx
  (Fn a1 a2, Fn b1 b2)          -> do
    ctx' <- subtype ctx b1 a1
    subtype ctx' (subst ctx' a2) (subst ctx' b2)
  (Forall u a, b) -> do
    alpha <- newE
    ctx'  <- extendMarker alpha ctx >>= extendE alpha
    ctx'' <- subtype ctx' (substEForU alpha u a) b
    let (beforeMarker, _) = splitAt (EVar alpha) ctx''
    pure beforeMarker
  (a, Forall u b) -> do
    ctx'  <- extendU u ctx
    ctx'' <- subtype ctx' a b
    let (beforeU, _) = splitAt (UVar u) ctx''
    pure beforeU
  (EType e, a) -> do
    guard (e `notElem` fv a)
    instantiateL ctx e a
  (a, EType e) -> do
    guard (e `notElem` fv a)
    instantiateR ctx a e
  _ -> liftMaybe Nothing

-- Instantiation
-- Existential vars are written e, f
-- Types are written a, b
instantiateL :: Ctx -> E -> Type -> TypeM Ctx
instantiateL ctx e = \case
  EType f -> do
    -- e must occur before f in the context
    -- to check this, we look for e after splitting the context at f
    let (l, r) = splitAt (EVar f) ctx
    void $ lookupE e l
    pure $ l <> [ESolved f (EType e)] <> r
  Fn a b -> do
    let (l, r) = splitAt (EVar e) ctx
    a1   <- newE
    a2   <- newE
    ctx' <- instantiateR
      (l <> [EVar a2, EVar a1, ESolved e (Fn (EType a1) (EType a2))] <> r)
      a
      a1
    instantiateL ctx' a2 (subst ctx' b)
  Forall u a -> do
    ctx'  <- extendU u ctx
    ctx'' <- instantiateL ctx' e a
    let (l, _) = splitAt (UVar u) ctx''
    pure l
  a -> do
    let (l, r) = splitAt (EVar e) ctx
    void $ wellFormedType l a
    pure $ l <> [ESolved e a] <> r

instantiateR :: Ctx -> Type -> E -> TypeM Ctx
instantiateR ctx ty e = case ty of
  EType f -> do
    -- e must occur before f in the context
    -- to check this, we look for e after splitting the context at f
    let (l, r) = splitAt (EVar f) ctx
    void $ lookupE e l
    pure $ l <> [ESolved f (EType e)] <> r
  Fn a b -> do
    let (l, r) = splitAt (EVar e) ctx
    a1   <- newE
    a2   <- newE
    ctx' <- instantiateL
      (l <> [EVar a2, EVar a1, ESolved e (Fn (EType a1) (EType a2))] <> r)
      a1
      a
    instantiateR ctx' (subst ctx' b) a2
  Forall u a -> do
    beta  <- newE
    ctx'  <- extendMarker beta ctx >>= extendE beta
    ctx'' <- instantiateR ctx' (substEForU beta u a) e
    let (l, _) = splitAt (Marker beta) ctx''
    pure l
  a -> do
    let (l, r) = splitAt (EVar e) ctx
    void $ wellFormedType l a
    pure $ l <> [ESolved e a] <> r

-- Typing
check :: Ctx -> Exp -> Type -> TypeM Ctx
check ctx expr ty = case (expr, ty) of
  (Lam x e, Fn a b) -> do
    ctx'  <- extendV x a ctx
    ctx'' <- check ctx' e b
    let (l, _) = splitAt (Var x a) ctx''
    pure l
  (UVal, Unit      ) -> pure ctx
  (e   , Forall u a) -> do
    ctx'  <- extendU u ctx
    ctx'' <- check ctx' e a
    let (l, _) = splitAt (UVar u) ctx''
    pure l
  (e, b) -> do
    (a, ctx') <- infer ctx e
    subtype ctx' (subst ctx' a) (subst ctx' b)

infer :: Ctx -> Exp -> TypeM (Type, Ctx)
infer ctx = \case
  VarExp x -> do
    a <- lookupV x ctx
    pure (a, ctx)
  Ann e a -> do
    void $ wellFormedType ctx a
    ctx' <- check ctx e a
    pure (a, ctx')
  UVal      -> pure (Unit, ctx)
  App e1 e2 -> do
    (a, ctx' ) <- infer ctx e1
    (c, ctx'') <- inferApp ctx' (subst ctx' a) e2
    pure (c, ctx'')
  Lam x e -> do
    alpha <- newE
    beta  <- newE
    ctx'  <- extendE alpha ctx >>= extendE beta >>= extendV x (EType alpha)
    ctx'' <- check ctx' e (EType beta)
    let (l, _) = splitAt (Var x (EType alpha)) ctx''
    pure (Fn (EType alpha) (EType beta), l)

inferApp :: Ctx -> Type -> Exp -> TypeM (Type, Ctx)
inferApp ctx ty e = case ty of
  Forall u a -> do
    alpha <- newE
    ctx'  <- extendE alpha ctx
    inferApp ctx' (substEForU alpha u a) e
  EType alpha -> do
    a1   <- newE
    a2   <- newE
    ctx' <- extendE a2 ctx >>= extendE a1 >>= extendSolved
      alpha
      (Fn (EType a1) (EType a2))
    ctx'' <- check ctx' e (EType a1)
    pure (EType a2, ctx'')
  Fn a b -> do
    ctx' <- check ctx e a
    pure (b, ctx')
  _ -> mzero

-- Tests

prop_uval_infers_unit :: Property
prop_uval_infers_unit = property $ do
  ctx <- forAll genCtx
  runTypeM (infer ctx UVal) === Just (Unit, ctx)

prop_infers_simple_app :: Property
prop_infers_simple_app = property $ do
  v <- forAll genV
  let expr = App (Ann (Lam v UVal) (Fn Unit Unit)) UVal
  runTypeM (infer emptyCtx expr) === Just (Unit, emptyCtx)

prop_infers_app_with_context :: Property
prop_infers_app_with_context = property $ do
  -- The context contains id : Unit -> Unit
  let ctx  = [Var (V 0) (Fn Unit Unit)]
  -- The expression is id UVal
  let expr = App (VarExp (V 0)) UVal
  -- The inferred type should be Unit
  runTypeM (infer ctx expr) === Just (Unit, ctx)

prop_infers_polymorphic_app :: Property
prop_infers_polymorphic_app = property $ do
  -- The context contains id     : forall a. a -> a
  --                      idUnit : Unit -> Unit
  let ctx =
        [ Var (V 0) (Forall (U 0) (Fn (UType (U 0)) (UType (U 0))))
        , Var (V 1) (Fn Unit Unit)
        ]
  -- The expression is idUnit (id UVal)
  let expr = App (VarExp (V 1)) (App (VarExp (V 0)) UVal)
  -- The inferred type should be Unit
  runTypeM (infer ctx expr) === Just (Unit, ctx)

tests :: Hedgehog.Group
tests = $$(Hedgehog.discover)
