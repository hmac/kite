{-# LANGUAGE TemplateHaskell #-}
module Type
  ( tests
  )
where

import           Util
import           Data.Name                      ( Name(..)
                                                , RawName(..)
                                                , ModuleName(..)
                                                )
import           Syn                            ( Pattern_(..) )

import           Prelude                 hiding ( splitAt )

import           Data.Maybe                     ( listToMaybe )
import           Control.Monad                  ( void )
import           Data.Functor                   ( ($>) )

import           Control.Monad.Trans.State.Strict
                                                ( State
                                                , evalState
                                                , get
                                                , put
                                                )
import           Control.Monad.Except           ( ExceptT(..)
                                                , runExceptT
                                                , throwError
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

-- Bidirectional typechecker
-- Following:
-- Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism

-- Primitive types
string :: Type
string = TCon "String" []

int :: Type
int = TCon "Int" []

char :: Type
char = TCon "Char" []

bool :: Type
bool = TCon "Bool" []

unit :: Type
unit = TCon "Unit" []

list :: Type -> Type
list a = TCon "List" [a]

tuple2 :: Type -> Type -> Type
tuple2 a b = TCon "Tuple2" [a, b]

primitiveConstructors :: Ctx
primitiveConstructors =
  [ Var (Free "Unit")  (TCon "Unit" [])
  , Var (Free "True")  (TCon "Bool" [])
  , Var (Free "False") (TCon "Bool" [])
  , Var (Free "[]")    (Forall (U 0) (TCon "List" [UType (U 0)]))
  , Var
    (Free "::")
    (Forall
      (U 0)
      (Fn (UType (U 0))
          (Fn (TCon "List" [UType (U 0)]) (TCon "List" [UType (U 0)]))
      )
    )
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
  | TCon Name [Type]
  -- Record type
  -- TODO: record typing rules
  | TRecord [(String, Type)]
  deriving (Eq, Show)

genType :: Gen Type
genType = G.recursive
  G.choice
  [EType <$> genE, UType <$> genU]
  [ G.subterm2 genType genType Fn
  , G.subtermM genType (\t -> Forall <$> genU <*> pure t)
  , TCon <$> genName <*> G.list (R.linear 0 3) genType
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
-- Guaranteed to be unique.
-- Contains a name hint for conversion back to source.
data V = Free Name
       | Bound Int
  deriving (Eq, Show)

genV :: Gen V
genV = G.choice [Free <$> genName, Bound <$> G.int (R.linear 0 100)]

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
  -- Data constructor
  | Con V
  -- Hole
  | Hole String
  -- -- Case expression
  | Case Exp [(Pattern, Exp)]
  deriving (Eq, Show)

type Pattern = Pattern_ V

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
  Fn     a b     -> fv a <> fv b
  Forall _ a     -> fv a
  EType e        -> [e]
  UType _        -> []
  TCon _ as      -> concatMap fv as
  TRecord fields -> concatMap (fv . snd) fields

prop_fv_commutes :: Property
prop_fv_commutes = property $ do
  a <- forAll genType
  b <- forAll genType
  fv (Fn a b) === (fv a <> fv b)

-- | Apply a context, as a substitution, to a type
subst :: Ctx -> Type -> Type
subst ctx = \case
  UType u        -> UType u
  Fn     a b     -> Fn (subst ctx a) (subst ctx b)
  Forall u a     -> Forall u (subst ctx a)
  TCon   c as    -> TCon c (map (subst ctx) as)
  TRecord fields -> TRecord $ mapSnd (subst ctx) fields
  EType   e      -> substE ctx
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
    Fn a b -> Fn (go a) (go b)
    Forall u' a | u == u'   -> Forall u' a
                | otherwise -> Forall u' (go a)
    EType e' -> EType e'
    UType u' | u == u'   -> EType e
             | otherwise -> UType u'
    TCon c as      -> TCon c (map go as)
    TRecord fields -> TRecord (mapSnd go fields)

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
lookupU u = liftMaybe1 (todoError "lookupU failed") . firstJust $ \case
  (UVar u') | u' == u -> Just u
  _                   -> Nothing

-- | Lookup an existential variable in the context
lookupE :: E -> Ctx -> TypeM E
lookupE e = liftMaybe1 (todoError "lookupE failed") . firstJust $ \case
  (EVar e') | e' == e -> Just e
  _                   -> Nothing

lookupSolved :: E -> Ctx -> TypeM (E, Type)
lookupSolved e =
  liftMaybe1 (todoError "lookupSolved failed") . firstJust $ \case
    (ESolved e' t) | e' == e -> Just (e', t)
    _                        -> Nothing

lookupV :: V -> Ctx -> TypeM Type
lookupV v ctx =
  liftMaybe (throwError (UnknownVariable ctx v)) $ flip firstJust ctx $ \case
    (Var v' t) | v' == v -> Just t
    _                    -> Nothing


-- Context construction

emptyCtx :: Ctx
emptyCtx = []

-- | Extend a context with a universal variable
extendU :: U -> Ctx -> TypeM Ctx
extendU u ctx | u `notElem` domU ctx = pure $ UVar u : ctx
              | otherwise            = todoError "extendU failed"

-- | Extend a context with a bound variable
extendV :: V -> Type -> Ctx -> TypeM Ctx
extendV v t ctx
  | v `notElem` domV ctx = wellFormedType ctx t >> pure (Var v t : ctx)
  | otherwise            = todoError "extendV failed"

-- | Extend a context with an existential variable
extendE :: E -> Ctx -> TypeM Ctx
extendE e ctx | e `notElem` domE ctx = pure $ EVar e : ctx
              | otherwise            = todoError "extendE failed"

-- | Extend a context with a solved existential variable
extendSolved :: E -> Type -> Ctx -> TypeM Ctx
extendSolved e t ctx
  | e `notElem` domE ctx = wellFormedType ctx t >> pure (ESolved e t : ctx)
  | otherwise            = todoError "extendSolved failed"

-- | Extend a context with an existential marker
extendMarker :: E -> Ctx -> TypeM Ctx
extendMarker e ctx
  | e `notElem` (domE ctx <> markers ctx) = pure $ Marker e : ctx
  | otherwise                             = todoError "extendMarker failed"

-- | Split a context at a given element, dropping that element
splitAt :: CtxElem -> Ctx -> (Ctx, Ctx)
splitAt e = \case
  []                   -> ([], [])
  (e' : ctx) | e' == e -> ([], ctx)
  (c : ctx)            -> let (l, r) = splitAt e ctx in (c : l, r)

-- | Drop all context elements after a particular element, inclusive
dropAfter :: CtxElem -> Ctx -> Ctx
dropAfter e = \case
  [] -> []
  (e' : ctx) | e' == e   -> ctx
             | otherwise -> dropAfter e ctx

-- Check if a type is well-formed
wellFormedType :: Ctx -> Type -> TypeM Type
wellFormedType ctx = \case
  Fn     a b -> wellFormedType ctx a >> wellFormedType ctx b
  Forall u t -> do
    ctx' <- extendU u ctx
    _    <- wellFormedType ctx' t
    pure $ Forall u t
  UType u -> lookupU u ctx >> pure (UType u)
  EType e -> do
    void (lookupE e ctx)
    pure (EType e)
  TCon c as -> TCon c <$> mapM (wellFormedType ctx) as
  TRecord fields ->
    TRecord <$> traverse (\(n, t) -> (n, ) <$> wellFormedType ctx t) fields

-- Typechecking monad
type TypeM = ExceptT Error (State Int)

runTypeM :: TypeM a -> Either Error a
runTypeM m = evalState (runExceptT m) 0

newInt :: TypeM Int
newInt = do
  i <- lift get
  lift $ put (i + 1)
  pure i

newE :: TypeM E
newE = E <$> newInt

-- | Lift a 'Maybe' value into the 'MaybeT' monad transformer.
liftMaybe :: TypeM a -> Maybe a -> TypeM a
liftMaybe err = maybe err pure

-- Lift a function returning Maybe into TypeM
liftMaybe1 :: TypeM b -> (a -> Maybe b) -> a -> TypeM b
liftMaybe1 err m x = maybe err pure (m x)

-- Subtyping
-- | Under this input context, the first type is a subtype of the second type,
-- with the given output context
subtype :: Ctx -> Type -> Type -> TypeM Ctx
subtype ctx typeA typeB = case (typeA, typeB) of
  (UType a, UType a') | a == a' -> lookupU a ctx $> ctx
  (EType a, EType a') | a == a' -> lookupE a ctx $> ctx
  (Fn a1 a2, Fn b1 b2)          -> do
    ctx' <- subtype ctx b1 a1
    subtype ctx' (subst ctx' a2) (subst ctx' b2)
  (Forall u a, b) -> do
    alpha <- newE
    ctx'  <- extendMarker alpha ctx >>= extendE alpha
    ctx'' <- subtype ctx' (substEForU alpha u a) b
    pure $ dropAfter (EVar alpha) ctx''
  (a, Forall u b) -> do
    ctx'  <- extendU u ctx
    ctx'' <- subtype ctx' a b
    pure $ dropAfter (UVar u) ctx''
  (EType e, a) | e `elem` fv a -> throwError $ OccursCheck e a
               | otherwise     -> instantiateL ctx e a
  (a, EType e) | e `elem` fv a -> throwError $ OccursCheck e a
               | otherwise     -> instantiateR ctx e a
  (TCon v as, TCon v' bs) | v == v' && as == bs -> pure ctx
  (a, b) -> throwError $ SubtypingFailure a b

-- Instantiation
-- Existential vars are written e, f
-- Types are written a, b
instantiateL :: Ctx -> E -> Type -> TypeM Ctx
instantiateL = instantiate (Left ())

instantiateR :: Ctx -> E -> Type -> TypeM Ctx
instantiateR = instantiate (Right ())

-- Either is used here as a cheap way of indicating if we're instantiating left
-- or right. This affects the rules for Fn and Forall.
instantiate :: Either () () -> Ctx -> E -> Type -> TypeM Ctx
instantiate dir ctx e ty = case ty of
  EType f -> do
    -- e must occur before f in the context
    -- to check this, we look for e after splitting the context at f
    let (l, r) = splitAt (EVar f) ctx
    void $ lookupE e r
    pure $ l <> [ESolved f (EType e)] <> r
  Fn a b -> do
    let (l, r) = splitAt (EVar e) ctx
    a1   <- newE
    a2   <- newE
    ctx' <- instantiate
      (flipDir dir)
      (l <> [ESolved e (Fn (EType a1) (EType a2)), EVar a1, EVar a2] <> r)
      a1
      a
    instantiate (flipDir dir) ctx' a2 (subst ctx' b)
  Forall u a -> case dir of
    Left _ -> do
      ctx'  <- extendU u ctx
      ctx'' <- instantiate dir ctx' e a
      pure $ dropAfter (UVar u) ctx''
    Right _ -> do
      beta  <- newE
      ctx'  <- extendMarker beta ctx >>= extendE beta
      ctx'' <- instantiate dir ctx' e (substEForU beta u a)
      pure $ dropAfter (Marker beta) ctx''
  a -> do
    let (l, r) = splitAt (EVar e) ctx
    void $ wellFormedType l a
    pure $ l <> [ESolved e a] <> r
 where
  flipDir :: Either () () -> Either () ()
  flipDir (Left  _) = Right ()
  flipDir (Right _) = Left ()

-- Typing
check :: Ctx -> Exp -> Type -> TypeM Ctx
check ctx expr ty = case (expr, ty) of
  (Lam x e, Fn a b) -> trace2 "check.fn" ctx $ do
    ctx'  <- extendV x a ctx
    ctx'' <- check ctx' e b
    pure $ dropAfter (Var x a) ctx''
  (e, Forall u a) -> trace2 "check.forall" ctx $ do
    ctx'  <- extendU u ctx
    ctx'' <- check ctx' e a
    pure $ dropAfter (UVar u) ctx''
  (Hole n, a) -> throwError $ CannotCheckHole (Hole n) a
  (e     , b) -> trace2 "check.default" ctx $ do
    (a, ctx') <- infer ctx e
    subtype ctx' (subst ctx' a) (subst ctx' b)

infer :: Ctx -> Exp -> TypeM (Type, Ctx)
infer ctx = \case
  VarExp x -> trace2 "infer.var" ctx $ do
    a <- lookupV x ctx
    pure (a, ctx)
  Ann e a -> trace2 "infer.ann" ctx $ do
    void $ wellFormedType ctx a
    ctx' <- check ctx e a
    pure (a, ctx')
  App e1 e2 -> trace2 "infer.app" ctx $ do
    (a, ctx' ) <- infer ctx e1
    (c, ctx'') <- inferApp ctx' (subst ctx' a) e2
    pure (c, ctx'')
  Lam x e -> trace2 "infer.lam" ctx $ do
    alpha <- newE
    beta  <- newE
    ctx'  <- extendE alpha ctx >>= extendE beta >>= extendV x (EType alpha)
    ctx'' <- check ctx' e (EType beta)
    pure (Fn (EType alpha) (EType beta), dropAfter (Var x (EType alpha)) ctx'')
  Hole n -> trace2 "infer.hole" ctx $ throwError $ CannotInferHole (Hole n)
  Con  x -> trace2 "infer.con" ctx $ do
    a <- lookupV x ctx
    pure (a, ctx)
  c@(  Case _ [])            -> throwError $ EmptyCase c
  Case scrut    (alt : alts) -> do
    (scrutTy, ctx' ) <- infer ctx scrut
    (altTy  , ctx'') <- inferAlt scrutTy ctx' alt
    ctx'''           <- foldM (checkAlt altTy scrutTy) ctx'' alts
    pure (altTy, ctx''')

-- TODO: probably better to infer the alts first, since they often constrain the
-- scrut type, and then we don't need to infer it.
inferAlt :: Type -> Ctx -> (Pattern, Exp) -> TypeM (Type, Ctx)
inferAlt scrutTy ctx (pat, expr) = do
  ctx' <- checkPattern ctx pat scrutTy
  infer ctx' expr

checkPattern :: Ctx -> Pattern -> Type -> TypeM Ctx
checkPattern ctx pat ty = case pat of
  WildPat     -> pure ctx
  VarPat  x   -> extendV x ty ctx
  IntPat  _   -> subtype ctx ty int
  CharPat _   -> subtype ctx ty char
  BoolPat _   -> subtype ctx ty bool
  UnitPat     -> subtype ctx ty unit
  StringPat _ -> subtype ctx ty string
  ConsPat con subpats ->
    second unfoldFn . unfoldForall <$> lookupV con ctx >>= \case
      (us, (argTys, TCon{})) -> do
        -- Create new existentials for each universal in the forall
        eSub <- mapM (\u -> (, u) <$> newE) us

        -- Add new existentials to the context
        ctx' <- foldM (flip extendU) ctx us

        -- Substitute them into the argtys
        let argTys' = map
              (\t -> foldl (\t' (e, u) -> substEForU e u t') t eSub)
              argTys

        -- Check each subpattern against the corresponding argty
        ctx'' <- foldM (\c (p, t) -> checkPattern c p t)
                       ctx'
                       (zip subpats argTys')
        -- Now we just return this context, I think?
        -- Nothing else to be done?
        pure ctx''
      (_, (_, t)) -> throwError $ ExpectedConstructorType t
  -- TODO: do subpats and list pats. convert them to conspats before checking
  ListPat  _ -> undefined
  TuplePat _ -> undefined

-- | unfoldForall (Forall a (Forall b t)) == ([a, b], t)
unfoldForall :: Type -> ([U], Type)
unfoldForall (Forall u t) = let (us, t') = unfoldForall t in (u : us, t')
unfoldForall t            = ([], t)

-- | unfoldFn (Fn a (Fn b (Fn c d))) = ([a, b, c], d)
unfoldFn :: Type -> ([Type], Type)
unfoldFn (Fn a b) = let (ts, t) = unfoldFn b in (a : ts, t)
unfoldFn t        = ([], t)

checkAlt :: Type -> Type -> Ctx -> (Pattern, Exp) -> TypeM Ctx
checkAlt expectedAltTy scrutTy ctx alt = do
  (inferredAltTy, ctx') <- inferAlt scrutTy ctx alt
  subtype ctx' expectedAltTy inferredAltTy

inferApp :: Ctx -> Type -> Exp -> TypeM (Type, Ctx)
inferApp ctx ty e = case ty of
  Forall u a -> trace2 "inferapp.forall" ctx $ do
    alpha <- newE
    ctx'  <- extendE alpha ctx
    inferApp ctx' (substEForU alpha u a) e
  EType alpha -> trace2 "inferapp.e" ctx $ do
    a1   <- newE
    a2   <- newE
    ctx' <- extendE a2 ctx >>= extendE a1 >>= extendSolved
      alpha
      (Fn (EType a1) (EType a2))
    ctx'' <- check ctx' e (EType a1)
    pure (EType a2, ctx'')
  Fn a b -> trace2 "inferapp.fn" ctx $ do
    ctx' <- check ctx e a
    pure (b, ctx')
  _ -> throwError $ InfAppFailure ty e

-- Type errors
data Error = TodoError String
           | OccursCheck E Type
           | OtherError Error
           | SubtypingFailure Type Type
           | InfAppFailure Type Exp
           | CannotInferHole Exp
           | CannotCheckHole Exp Type
           | UnknownVariable Ctx V
           | EmptyCase Exp
           | ExpectedConstructorType Type
           deriving (Eq, Show)

todoError :: String -> TypeM a
todoError = throwError . TodoError

-- Tests

prop_uval_infers_unit :: Property
prop_uval_infers_unit = property $ do
  ctx <- (primitiveConstructors <>) <$> forAll genCtx
  let expr = Con (Free "Unit")
      ty   = TCon "Unit" []
  runTypeM (infer ctx expr) === Right (ty, ctx)

prop_checks_bad_unit_annotation :: Property
prop_checks_bad_unit_annotation = property $ do
  ctx <- (primitiveConstructors <>) <$> forAll genCtx
  let expr = Con (Free "Unit")
      ty   = Fn unit unit
  runTypeM (check ctx expr ty) === Left (SubtypingFailure unit (Fn unit unit))

prop_infers_simple_app :: Property
prop_infers_simple_app = property $ do
  let ctx = primitiveConstructors
  v <- forAll genV
  let uval = Con (Free "Unit")
  let expr = App (Ann (Lam v uval) (Fn unit unit)) uval
  runTypeM (infer ctx expr) === Right (unit, ctx)

prop_infers_app_with_context :: Property
prop_infers_app_with_context = property $ do
  -- The context contains id : Unit -> Unit
  let uval = Con (Free "Unit")
      ctx  = [Var (Free "id") (Fn unit unit)] <> primitiveConstructors
  -- The expression is id Unit
  let expr = App (VarExp (Free "id")) uval
  -- The inferred type should be Unit
  runTypeM (infer ctx expr) === Right (unit, ctx)

prop_infers_polymorphic_app :: Property
prop_infers_polymorphic_app = property $ do
  -- The context contains id     : forall a. a -> a
  --                      idUnit : Unit -> Unit
  let uval = Con (Free "Unit")
      ctx =
        [ Var (Free "id") (Forall (U 0) (Fn (UType (U 0)) (UType (U 0))))
          , Var (Free "idUnit") (Fn unit unit)
          ]
          <> primitiveConstructors
  -- The expression is idUnit (id Unit)
  let expr = App (VarExp (Free "idUnit")) (App (VarExp (Free "id")) uval)
  -- The inferred type should be Unit
  runTypeM (infer ctx expr) === Right (unit, ESolved (E 0) unit : ctx)

prop_infers_list_app :: Property
prop_infers_list_app = property $ do
  let ty   = Forall (U 0) (TCon "List" [UType (U 0)])
      -- [] : forall a. List a
      ctx  = [Var (Free "[]") ty]
      expr = VarExp (Free "[]")
  runTypeM (infer ctx expr) === Right (ty, ctx)

prop_infers_bool_case :: Property
prop_infers_bool_case = property $ do
  let ctx   = primitiveConstructors
      expr1 = Case
        (Con (Free "True"))
        [(BoolPat True, Con (Free "Unit")), (BoolPat False, Con (Free "Unit"))]
      expr2 = Case
        (Con (Free "True"))
        [(BoolPat True, Con (Free "True")), (BoolPat False, Con (Free "Unit"))]
  runTypeM (infer ctx expr1) === Right (unit, ctx)
  runTypeM (infer ctx expr2) === Left (SubtypingFailure bool unit)

tests :: Hedgehog.Group
tests = $$(Hedgehog.discover)

-- Util functions
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

genName :: Gen Name
genName = G.choice
  [Local <$> genLowerRawName, TopLevel <$> genModuleName <*> genLowerRawName]

-- Uppercase names are data constructors, so are always qualified
genUpperName :: Gen Name
genUpperName = G.choice [TopLevel <$> genModuleName <*> genUpperRawName]

genModuleName :: Gen ModuleName
genModuleName = ModuleName <$> G.list (R.linear 1 3) genUpperString

genUpperString :: Gen String
genUpperString = do
  c  <- G.upper
  cs <- G.list (R.linear 0 10) G.alphaNum
  pure (c : cs)

genLowerRawName :: Gen RawName
genLowerRawName = Name <$> genLowerString

genUpperRawName :: Gen RawName
genUpperRawName = Name <$> genUpperString

genLowerString :: Gen String
genLowerString = do
  c  <- G.lower
  cs <- G.list (R.linear 0 5) G.alphaNum
  pure (c : cs)

trace2 :: String -> Ctx -> a -> a
trace2 msg ctx = trace (msg <> " " <> pShow (length ctx))
