{-# LANGUAGE TemplateHaskell #-}
module Type
  ( tests
  , Ctx
  , CtxElem(..)
  , Type(..)
  , Exp(..)
  , V(..)
  , U(..)
  , Pattern
  , Pattern_(..)
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
  )
where

import           Util
import           Data.List                      ( intercalate )
import           Data.String                    ( fromString )
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
import qualified Control.Monad.Except
import           Control.Monad.Except           ( ExceptT(..)
                                                , runExceptT
                                                , catchError
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , runReaderT
                                                , ask
                                                , asks
                                                , local
                                                )

import qualified Hedgehog.Gen                  as G
import qualified Hedgehog.Range                as R
import           Hedgehog                       ( Property
                                                , property
                                                , Gen
                                                , (===)
                                                , forAll
                                                )
import qualified Hedgehog

import           Type.Reflection                ( Typeable )
import           Data.Data                      ( Data )

-- Bidirectional typechecker
-- Following:
-- Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism

-- Primitive types
string :: Type
string = TCon "Lam.Primitive.String" []

int :: Type
int = TCon "Lam.Primitive.Int" []

char :: Type
char = TCon "Lam.Primitive.Char" []

bool :: Type
bool = TCon "Lam.Primitive.Bool" []

unit :: Type
unit = TCon "Lam.Primitive.Unit" []

list :: Type -> Type
list a = TCon "Lam.Primitive.List" [a]

io :: Type
io = TCon "Lam.Primitive.IO" []

-- Primitive constructors
primitiveConstructors :: Ctx
primitiveConstructors =
  [ Var (Free "Lam.Primitive.Unit")  unit
  , Var (Free "Lam.Primitive.True")  bool
  , Var (Free "Lam.Primitive.False") bool
  , Var (Free "Lam.Primitive.[]") (Forall (U 0 "a") (list (UType (U 0 "a"))))
  , Var
    (Free "Lam.Primitive.::")
    (Forall
      (U 0 "a")
      (Fn (UType (U 0 "a"))
          (Fn (list (UType (U 0 "a"))) (list (UType (U 0 "a"))))
      )
    )
  , Var (Free "Lam.Primitive.Tuple2") (mkTupleCon 2 "Lam.Primitive.Tuple2")
  , Var (Free "Lam.Primitive.Tuple3") (mkTupleCon 3 "Lam.Primitive.Tuple3")
  , Var (Free "Lam.Primitive.Tuple4") (mkTupleCon 4 "Lam.Primitive.Tuple4")
  , Var (Free "Lam.Primitive.Tuple5") (mkTupleCon 5 "Lam.Primitive.Tuple5")
  , Var (Free "Lam.Primitive.Tuple6") (mkTupleCon 6 "Lam.Primitive.Tuple6")
  , Var (Free "Lam.Primitive.Tuple7") (mkTupleCon 7 "Lam.Primitive.Tuple7")
  , Var (Free "Lam.Primitive.Tuple8") (mkTupleCon 8 "Lam.Primitive.Tuple8")
  ]

-- Primitive functions
primitiveFns :: Ctx
primitiveFns =
  [ Var (Free "Lam.Primitive.appendString") (Fn string (Fn string string))
  , Var (Free "Lam.Primitive.+")            (Fn int (Fn int int))
  , Var (Free "Lam.Primitive.-")            (Fn int (Fn int int))
  , Var (Free "Lam.Primitive.*")            (Fn int (Fn int int))
  , Var (Free "Lam.Primitive.$showInt")     (Fn int string)
  , Var (Free "Lam.Primitive.$eqInt")       (Fn int (Fn int bool))
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
  [ ("putStrLn", Fn string (TApp io [unit]))
  , ("putStr"  , Fn string (TApp io [unit]))
  , ("getLine" , TApp io [string])
  , ( "pureIO"
    , Forall (U 0 "a") (Fn (UType (U 0 "a")) (TApp io [UType (U 0 "a")]))
    )
  , ( "bindIO"
    , Forall (U 0 "a") $ Forall (U 1 "b") $ Fn (TApp io [UType (U 0 "a")]) $ Fn
      (Fn (UType (U 0 "a")) (TApp io [UType (U 1 "b")]))
      (TApp io [UType (U 1 "b")])
    )
  , ("chars"   , Fn string (list char))
  , ("showChar", Fn char string)
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
-- Contains a name hint
data U = U Int Name
  deriving (Show, Typeable, Data)

instance Eq U where
  (U i _) == (U j _) = i == j

instance Debug U where
  debug (U n v) = debug v <> show n

genU :: Gen U
genU = U <$> G.int (R.linear 0 100) <*> genName

-- Existential type variable
newtype E = E Int
  deriving (Eq, Show, Typeable, Data)

instance Debug E where
  debug (E e) = "e" <> show e

genE :: Gen E
genE = E <$> G.int (R.linear 0 100)

-- Free or bound variable
-- Guaranteed to be unique.
-- Contains a name hint for conversion back to source.
data V = Free Name
       | Bound Int -- Not currently used, but should be for lambda bindings
  deriving (Eq, Show, Typeable, Data)

instance Debug V where
  debug (Free  n) = debug n
  debug (Bound i) = "v" <> show i

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
  -- Case expression
  | Case Exp [(Pattern, Exp)]
  -- MCase expression
  | MCase [([Pattern], Exp)]
  -- Multi let expression
  | Let [(V, Exp)] Exp
  -- String literal
  | String String
  -- Char literal
  | Char Char
  -- Int literal
  | Int Int
  -- Bool literal
  | Bool Bool
  -- Unit literal
  | Unit
  -- List literal
  | List [Exp]
  -- Tuple literal
  | Tuple [Exp]
  -- Record
  | Record [(String, Exp)]
  -- Record projection
  | Project Exp String
  -- FFI Call
  | FCall String [Exp]
  deriving (Eq, Show, Typeable, Data)

instance Debug Exp where
  debug (VarExp v) = debug v
  debug (Ann e t ) = debug e <+> ":" <+> debug t
  debug (App a b ) = debug a <+> debug b
  debug (Lam v e ) = "λ" <> debug v <> "." <+> debug e
  debug (Con  v  ) = debug v
  debug (Hole s  ) = "?" <> s
  debug (Case e alts) =
    "case" <+> debug e <+> "of {" <+> sepBy "; " (map go alts) <+> "}"
    where go (pat, expr) = debug pat <+> "->" <+> debug expr
  debug (MCase alts) = "mcase" <+> "{" <+> sepBy "; " (map go alts) <+> "}"
    where go (pats, expr) = sepBy " " (map debug pats) <+> "->" <+> debug expr
  debug (Let binds e) =
    "let"
      <+> foldl (\acc (x, a) -> debug x <+> "=" <+> debug a <> ";" <+> acc)
                ("in" <+> debug e)
                binds
  debug (String s     ) = "\"" <> s <> "\""
  debug (Char   c     ) = "'" <> [c] <> "'"
  debug (Int    i     ) = show i
  debug (Bool   b     ) = show b
  debug (Unit         ) = "()"
  debug (List   elems ) = "[" <+> sepBy ", " (map debug elems) <+> "]"
  debug (Tuple  elems ) = "(" <+> sepBy ", " (map debug elems) <+> ")"
  debug (Record fields) = "{" <+> sepBy ", " (map go fields) <+> "}"
    where go (name, expr) = debug name <+> "=" <+> debug expr
  debug (Project r f   ) = debug r <> "." <> f
  debug (FCall   f args) = "$fcall" <+> f <+> sepBy " " (map debug args)

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

instance Debug CtxElem where
  debug (Var v t     ) = debug v <+> ":" <+> debug t
  debug (UVar u      ) = debug u
  debug (EVar e      ) = debug e
  debug (ESolved e ty) = debug e <+> "=" <+> debug ty
  debug (Marker e    ) = "'" <> debug e

debugCtx :: Ctx -> String
debugCtx ctx = "[" <> sepBy ", " (map debug ctx) <> "]"


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
  TApp f as      -> fv f <> concatMap fv as

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
  -- If the substitution yields a nested TCon or TApp, flatten it back to spine
  -- form.
  TApp f as ->
    let as' = map (subst ctx) as
    in  case subst ctx f of
          TCon c args -> TCon c (args <> as')
          TApp g cs   -> TApp g (cs <> as')
          f'          -> TApp f' as'
  EType e -> substE ctx
   where
    substE :: Ctx -> Type
    substE = \case
      []                              -> EType e
      (ESolved e' t : ctx') | e == e' -> subst ctx' t
      (EVar e' : _) | e == e'         -> EType e
      (_ : c)                         -> substE c

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
    TApp f as      -> TApp (go f) (map go as)

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
lookupU u =
  liftMaybe1 (todoError ("lookupU failed: " <> show u)) . firstJust $ \case
    (UVar u') | u' == u -> Just u
    _                   -> Nothing

-- | Lookup an existential variable in the context
lookupE :: E -> Ctx -> TypeM E
lookupE e =
  liftMaybe1 (todoError ("lookupE failed: " <> show e)) . firstJust $ \case
    (EVar e') | e' == e -> Just e
    _                   -> Nothing

lookupSolved :: E -> Ctx -> TypeM (E, Type)
lookupSolved e =
  liftMaybe1 (todoError "lookupSolved failed") . firstJust $ \case
    (ESolved e' t) | e' == e -> Just (e', t)
    _                        -> Nothing

-- | Lookup a normal variable in the context.
-- If it's not in the local context, we check the global context.
lookupV :: V -> Ctx -> TypeM Type
lookupV v ctx = do
  TypeEnv { envCtx = globalCtx } <- ask
  liftMaybe (throwError (UnknownVariable v))
    $ flip firstJust (ctx <> globalCtx)
    $ \case
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
  | otherwise            = todoError $ "extendV failed:" <+> debug v <+> debug t

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
wellFormedType ctx ty = trace' ctx ["wellFormedType", debug ty] $ case ty of
  Fn     a b -> Fn <$> wellFormedType ctx a <*> wellFormedType ctx b
  Forall u t -> do
    ctx' <- extendU u ctx
    _    <- wellFormedType ctx' t
    pure $ Forall u t
  UType u -> lookupU u ctx >> pure (UType u)
  EType e -> do
    void (lookupE e ctx) `catchError` const (void (lookupSolved e ctx))
    pure (EType e)
  -- TODO: check that c exists
  TCon c as -> TCon c <$> mapM (wellFormedType ctx) as
  TRecord fields ->
    TRecord <$> traverse (\(n, t) -> (n, ) <$> wellFormedType ctx t) fields
  TApp f b -> TApp <$> wellFormedType ctx f <*> mapM (wellFormedType ctx) b

-- Typechecking monad
data TypeEnv =
  TypeEnv { envCtx :: Ctx    -- The global type context
          , envDepth :: Int  -- The recursion depth, used for debugging
          , envDebug :: Bool -- Whether to output debug messages
          } deriving (Eq, Show)

defaultTypeEnv :: TypeEnv
defaultTypeEnv = TypeEnv { envCtx = primCtx, envDepth = 0, envDebug = False }

type TypeM = ReaderT TypeEnv (ExceptT LocatedError (State Int))

runTypeM :: TypeEnv -> TypeM a -> Either LocatedError a
runTypeM env m = evalState (runExceptT (runReaderT m env)) 0

-- Increment the recursion depth
incDepth :: TypeEnv -> TypeEnv
incDepth t = t { envDepth = (envDepth t) + 1 }

newInt :: TypeM Int
newInt = do
  i <- lift (lift get)
  lift $ lift $ put (i + 1)
  pure i

newE :: TypeM E
newE = E <$> newInt

newU :: Name -> TypeM U
newU hint = U <$> newInt <*> pure hint

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
subtype ctx typeA typeB =
  trace' ctx ["subtype", debug typeA, debug typeB] $ case (typeA, typeB) of
    (UType a, UType a') | a == a' -> lookupU a ctx $> ctx
    (EType a, EType a') | a == a' -> lookupE a ctx $> ctx
    (Fn a1 a2, Fn b1 b2)          -> do
      ctx' <- subtype ctx b1 a1
      subtype ctx' (subst ctx' a2) (subst ctx' b2)
    (a, Forall u b) -> do
      ctx'  <- extendU u ctx
      ctx'' <- subtype ctx' a b
      pure $ dropAfter (UVar u) ctx''
    (Forall u a, b) -> do
      alpha <- newE
      ctx'  <- extendMarker alpha ctx >>= extendE alpha
      ctx'' <- subtype ctx' (substEForU alpha u a) b
      pure $ dropAfter (Marker alpha) ctx''
    (EType e, a) | e `elem` fv a -> throwError $ OccursCheck e a
                 | otherwise     -> instantiateL ctx e a
    (a, EType e) | e `elem` fv a -> throwError $ OccursCheck e a
                 | otherwise     -> instantiateR ctx e a
    (TCon v as, TCon v' bs) | v == v' ->
      foldM (\c (a, b) -> subtype c a b) ctx (zip as bs)
    (TCon c as, TApp v bs) -> do
      ctx' <- subtype ctx (TCon c []) v
      foldM (\ctx_ (a, b) -> subtype ctx_ a b) ctx' (zip as bs)
    (TApp v as, TCon c bs) -> do
      ctx' <- subtype ctx v (TCon c [])
      foldM (\ctx_ (a, b) -> subtype ctx_ a b) ctx' (zip as bs)
    (TApp v as, TApp u bs) -> do
      ctx' <- subtype ctx v u
      foldM (\c (a, b) -> subtype c a b) ctx' (zip as bs)
    (TRecord as, TRecord bs) -> do
      -- For any records A, B
      -- A < B if every field fB in B has a corresponding field fA such that fA < fB
      foldM
        (\ctx_ (label, bTy) -> case lookup label bs of
          Nothing  -> throwError $ SubtypingFailure (TRecord as) (TRecord bs)
          Just aTy -> subtype ctx_ aTy bTy
        )
        ctx
        bs


    (a, b) -> throwError $ SubtypingFailure (subst ctx a) (subst ctx b)

-- Instantiation
-- Existential vars are written e, f
-- Types are written a, b
instantiateL :: Ctx -> E -> Type -> TypeM Ctx
instantiateL = instantiate L

instantiateR :: Ctx -> E -> Type -> TypeM Ctx
instantiateR = instantiate R

data InstDir = L | R deriving (Eq, Show)

instantiate :: InstDir -> Ctx -> E -> Type -> TypeM Ctx
instantiate dir ctx e ty =
  trace' ctx ["inst", show dir, debug e, debug ty] $ case ty of
    EType f ->
      -- Here we want to instantiate either e := f or f := e. Which one depends
      -- on which variable has a greater scope - i.e. appears first in the
      -- context. We are only allowed to instantiate a variable to another
      -- variable which appears before it in the context.

      -- First, assume that e occurs before f.
      -- Split the context at f, and look for e in the prefix.
      (do
          let (l, r) = splitAt (EVar f) ctx
          void $ lookupE e r
          -- If we find it, instantiate f := e and we're done
          pure $ l <> [ESolved f (EType e)] <> r
        )
        `catchError` (\_ -> do
                       -- e isn't in the prefix, so assume that f occurs before e
                       -- Split the context at e, and look for f in the prefix
                       let (l, r) = splitAt (EVar e) ctx
                       void $ lookupE f r
                       -- If we find it, instantiate f := e and we're done.
                       -- Otherwise, throw an error
                       pure $ l <> [ESolved e (EType f)] <> r
                     )
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
      L -> do
        ctx'  <- extendU u ctx
        ctx'' <- instantiate dir ctx' e a
        pure $ dropAfter (UVar u) ctx''
      R -> do
        beta  <- newE
        ctx'  <- extendMarker beta ctx >>= extendE beta
        ctx'' <- instantiate dir ctx' e (substEForU beta u a)
        pure $ dropAfter (Marker beta) ctx''
    TCon c [EType b] -> do
      -- hmac: custom rule for type applications
      -- G [e3, e1 = A e3][e2] |- e2 :=< e3   -| G'
      -- ------------------------------------------
      -- G[e1][e2]             |- A e2 <=: e1 -| G'
      --
      -- TODO: how do we handle multi-arg applications?
      let (l, r) = splitAt (EVar e) ctx
      e3 <- newE
      let ctx' = l <> [ESolved e (TCon c [EType e3]), EVar e3] <> r
      instantiate (flipDir dir) ctx' b (EType e3)
    a -> do
      let (l, r) = splitAt (EVar e) ctx
      void $ wellFormedType r a
      pure $ l <> [ESolved e a] <> r
 where
  flipDir :: InstDir -> InstDir
  flipDir L = R
  flipDir R = L

-- Typing
check :: Ctx -> Exp -> Type -> TypeM Ctx
check ctx expr ty =
  trace' ctx ["check", debug expr, ":", debug ty] $ case (expr, ty) of
    (Lam x e, Fn a b) -> do
      ctx'  <- extendV x a ctx
      ctx'' <- check ctx' e b
      pure $ dropAfter (Var x a) ctx''
    (e, Forall u a) -> do
      ctx'  <- extendU u ctx
      ctx'' <- check ctx' e a
      pure $ dropAfter (UVar u) ctx''
    (Hole n        , a) -> throwError $ CannotCheckHole (Hole n) a
    (Let binds body, _) -> do
      -- generate a dummy existential that we'll use to cut the context
      alpha <- newE
      ctx'  <- extendMarker alpha ctx

      -- infer the type of each binding, adding to the context as we go
      ctx'' <- foldM (\c (x, e) -> infer c e >>= \(a, c1) -> extendV x a c1)
                     ctx'
                     (reverse binds)

      -- check the type of the body
      ctx''' <- check ctx'' body ty

      -- drop all these variables off the context
      pure $ dropAfter (Marker alpha) ctx'''
    (FCall name args, _) -> do
      case lookup name fcallInfo of
        Just fCallTy -> do
          (resultTy, ctx') <- foldM (\(fTy, c) arg -> inferApp c fTy arg)
                                    (fCallTy, ctx)
                                    args
          subtype ctx' (subst ctx' resultTy) ty
        Nothing -> throwError $ UnknownFCall name
    (MCase []                  , _     ) -> throwError (EmptyCase expr)
    (MCase alts@((pats, _) : _), Fn a b) -> do
      -- Split off as many args as there are patterns in the first alt
      let (argTys, exprTy) =
            let (as, b') = unfoldFn (Fn a b)
            in  (take (length pats) as, foldFn (drop (length pats) as) b')
      foldM (checkMCaseAlt argTys exprTy) ctx alts
    (e, b) -> do
      (a, ctx') <- infer ctx e
      subtype ctx' (subst ctx' a) (subst ctx' b)

checkMCaseAlt :: [Type] -> Type -> Ctx -> ([Pattern], Exp) -> TypeM Ctx
checkMCaseAlt patTys _ _ (pats, _) | length patTys /= length pats =
  throwError TooManyPatterns
checkMCaseAlt patTys rhsTy ctx (pats, rhs) = do
  -- generate a dummy existential that we'll use as a marker to know where to
  -- split the context, at the end of this function
  alpha <- newE
  ctx'  <- extendMarker alpha ctx

  -- check each pat against the corresponding pat type, accumulating a new
  -- context
  ctx'' <- foldM (\ctx_ (pat, ty) -> checkPattern ctx_ pat ty)
                 ctx'
                 (zip pats patTys)

  -- check the rhs against the rhs type
  ctx''' <- check ctx'' rhs rhsTy

  -- drop all context elements after and including the marker
  pure $ dropAfter (Marker alpha) ctx'''

-- Like infer but applies the resulting substitution to the type and returns
-- just the type.
-- Useful for tests.
infer' :: Ctx -> Exp -> TypeM Type
infer' ctx e = trace' ctx ["infer'", debug e] $ do
  (ty, ctx') <- infer ctx e
  pure $ subst ctx' ty

-- TODO: flip this around so it's Ctx -> Exp -> TypeM (Ctx, Typ)
-- This fits with foldM and mapAccumLM
infer :: Ctx -> Exp -> TypeM (Type, Ctx)
infer ctx expr_ = trace' ctx ["infer", debug expr_] $ case expr_ of
  VarExp x -> do
    a <- lookupV x ctx
    pure (a, ctx)
  Ann e a -> do
    void $ wellFormedType ctx a
    ctx' <- check ctx e a
    pure (a, ctx')
  App e1 e2 -> do
    (a, ctx' ) <- infer ctx e1
    (c, ctx'') <- inferApp ctx' (subst ctx' a) e2
    pure (c, ctx'')
  Lam x e -> do
    alpha <- newE
    beta  <- newE
    ctx'  <- extendE alpha ctx >>= extendE beta >>= extendV x (EType alpha)
    ctx'' <- check ctx' e (EType beta)
    pure (Fn (EType alpha) (EType beta), dropAfter (Var x (EType alpha)) ctx'')
  Hole n -> throwError $ CannotInferHole (Hole n)
  Con  x -> do
    a <- lookupV x ctx
    pure (a, ctx)
  c@(  Case _ [])            -> throwError $ EmptyCase c
  Case scrut    (alt : alts) -> do
    (scrutTy, ctx' ) <- infer ctx scrut
    (altTy  , ctx'') <- inferCaseAlt scrutTy ctx' alt
    ctx'''           <- foldM (checkCaseAlt altTy scrutTy) ctx'' alts
    pure (altTy, ctx''')
  c@(MCase [])                -> throwError $ EmptyCase c
  MCase ((pats, expr) : alts) -> do
    -- The type of an mcase will be a function type taking as many arguments as
    -- there are patterns. Each alt should have the same number of patterns.
    -- Taking the first alt, we infer a type for each pattern and a type for the
    -- RHS expr. We then check the remaining alts against this. This yields a
    -- function type which we return. It may have existential variables that
    -- need to be resolved later on.

    -- First, we infer a type for each pattern in the first alt
    (ctx'  , patTys) <- mapAccumLM inferPattern ctx pats
    -- Next, infer a type for the RHS
    (exprTy, ctx'' ) <- infer ctx' expr
    -- Now check the remaining alts using this information
    ctx'''           <- foldM (checkMCaseAlt patTys exprTy) ctx'' alts
    -- Now construct a result type and return it
    pure (foldFn patTys exprTy, ctx''')
  Let binds body -> do
    -- generate a dummy existential that we'll use to cut the context
    alpha <- newE
    ctx'  <- extendMarker alpha ctx

    -- infer the type of each binding, adding to the context as we go
    ctx'' <- foldM (\c (x, e) -> infer c e >>= \(a, c1) -> extendV x a c1)
                   ctx'
                   (reverse binds)
    -- infer the type of the body
    (ty, ctx''') <- infer ctx'' body

    -- drop all these variables off the context
    pure (subst ctx''' ty, dropAfter (Marker alpha) ctx''')
  String _   -> pure (string, ctx)
  Char   _   -> pure (char, ctx)
  Int    _   -> pure (int, ctx)
  Bool   _   -> pure (bool, ctx)
  Unit       -> pure (unit, ctx)
  List elems -> do
    -- Construct a nested application of (::) and [] and infer the type of that
    let expr = foldr
          (\s acc -> App (App (Con (Free "Lam.Primitive.::")) s) acc)
          (Con (Free "Lam.Primitive.[]"))
          elems
    infer ctx expr
  Tuple elems -> do
    -- Construct an application of TupleN and infer the type of that
    let n    = length elems
        con  = Con (Free (fromString ("Lam.Primitive.Tuple" <> show n)))
        expr = foldl App con elems
    infer ctx expr
  Record fields -> do
    -- To infer the type of a record, we must be able to infer types for all its
    -- fields
    (ctx', fTypes) <- mapAccumLM
      (\ctx_ (name, expr) -> do
        (ty, ctx_') <- infer ctx_ expr
        pure (ctx_', (name, subst ctx_' ty))
      )
      ctx
      fields
    pure (TRecord fTypes, ctx')
  Project record fieldName -> do
    -- To infer the type of a record projection, we must know the type of the
    -- record.
    (recordTy, ctx') <- infer ctx record
    -- Check if the record contains this field
    case subst ctx' recordTy of
      TRecord fields -> case lookup fieldName fields of
        Just fieldTy -> pure (fieldTy, ctx')
        Nothing      -> throwError $ RecordDoesNotHaveField recordTy fieldName
      _ -> throwError $ NotARecordType recordTy
  e@(FCall _name _args) -> throwError $ CannotInferFCall e

-- TODO: check that we have the same number of scrutinee types as patterns
inferMCaseAlt :: [Type] -> Ctx -> ([Pattern], Exp) -> TypeM (Type, Ctx)
inferMCaseAlt scrutTys ctx (pats, expr) = do
  ctx' <- foldM (\c (pat, ty) -> checkPattern c pat ty) ctx (zip pats scrutTys)
  infer ctx' expr

-- TODO: probably better to infer the alts first, since they often constrain the
-- scrut type, and then we don't need to infer it.
inferCaseAlt :: Type -> Ctx -> (Pattern, Exp) -> TypeM (Type, Ctx)
inferCaseAlt scrutTy ctx (pat, expr) =
  trace' ctx ["inferCaseAlt", debug pat, debug expr] $ do
    ctx' <- checkPattern ctx pat scrutTy
    infer ctx' expr

inferPattern :: Ctx -> Pattern -> TypeM (Ctx, Type)
inferPattern ctx pattern =
  trace' ctx ["inferPattern", debug pattern] $ case pattern of
    IntPat  _           -> pure (ctx, int)
    CharPat _           -> pure (ctx, char)
    BoolPat _           -> pure (ctx, bool)
    UnitPat             -> pure (ctx, unit)
    StringPat _         -> pure (ctx, string)
    ConsPat con subpats -> do
        -- Lookup the type of the constructor
      conTy <- lookupV con ctx
      case second unfoldFn (unfoldForall conTy) of
        (us, (argTys, tcon@TCon{})) -> do
          -- Create new existentials for each universal in the forall
          -- Add them to the context
          eSub <- mapM (\u -> (, u) <$> newE) us
          ctx' <- foldM (flip extendE) ctx (map fst eSub)

          -- Substitute them into the argtys and the tcon
          let
            argTys' =
              map (\t -> foldl (\t' (e, u) -> substEForU e u t') t eSub) argTys
            tcon' = foldl (\t' (e, u) -> substEForU e u t') tcon eSub

          -- Check each subpattern against the corresponding argty
          ctx'' <- foldM (\c (p, t) -> checkPattern c p t)
                         ctx'
                         (zip subpats argTys')

          -- Return the constructor type
          pure (ctx'', tcon')
        (_, (_, t)) -> throwError $ ExpectedConstructorType t
    VarPat x -> do
      alpha <- newE
      ctx'  <- extendE alpha ctx >>= extendV x (EType alpha)
      pure (ctx', EType alpha)
    WildPat -> do
      alpha <- newE
      ctx'  <- extendE alpha ctx
      pure (ctx', EType alpha)
    TuplePat subpats ->
      let
        con = case subpats of
          []                       -> error "Type.inferPattern: empty tuple"
          [_] -> error "Type.inferPattern: single-element tuple"
          [_, _]                   -> Free "Lam.Primitive.Tuple2"
          [_, _, _]                -> Free "Lam.Primitive.Tuple3"
          [_, _, _, _]             -> Free "Lam.Primitive.Tuple4"
          [_, _, _, _, _]          -> Free "Lam.Primitive.Tuple5"
          [_, _, _, _, _, _]       -> Free "Lam.Primitive.Tuple6"
          [_, _, _, _, _, _, _]    -> Free "Lam.Primitive.Tuple7"
          [_, _, _, _, _, _, _, _] -> Free "Lam.Primitive.Tuple8"
          _ ->
            error
              $  "Type.inferPattern: cannot (yet) handle tuples of length > 8: "
              <> show subpats
      in  inferPattern ctx (ConsPat con subpats)
    ListPat []      -> inferPattern ctx (ConsPat (Free "Lam.Primitive.[]") [])
    ListPat subpats -> inferPattern
      ctx
      (foldr (\s acc -> ConsPat (Free "Lam.Primitive.::") [s, acc])
             (ConsPat (Free "Lam.Primitive.[]") [])
             subpats
      )

checkPattern :: Ctx -> Pattern -> Type -> TypeM Ctx
checkPattern ctx pat ty =
  trace' ctx ["checkPattern", debug pat, ":", debug ty] $ case pat of
    WildPat             -> pure ctx
    VarPat  x           -> extendV x ty ctx
    IntPat  _           -> subtype ctx ty int
    CharPat _           -> subtype ctx ty char
    BoolPat _           -> subtype ctx ty bool
    UnitPat             -> subtype ctx ty unit
    StringPat _         -> subtype ctx ty string
    ConsPat con subpats -> do
      constructorType <- lookupV con ctx
      case second unfoldFn (unfoldForall constructorType) of
        (us, (argTys, tcon@TCon{})) -> do
          -- Create new existentials for each universal in the forall
          eSub <- mapM (\u -> (, u) <$> newE) us

          -- Add new existentials to the context
          ctx' <- foldM (flip extendE) ctx (map fst eSub)

          -- Substitute them into the argtys and the tcon
          let
            argTys' =
              map (\t -> foldl (\t' (e, u) -> substEForU e u t') t eSub) argTys
            tcon' = foldl (\t' (e, u) -> substEForU e u t') tcon eSub

          -- Check each subpattern against the corresponding argty
          ctx'' <- foldM (\c (p, t) -> checkPattern c p t)
                         ctx'
                         (zip subpats argTys')

          -- Check this against the type we have been given
          ctx''' <- subtype ctx'' (subst ctx'' tcon') (subst ctx'' ty)

          pure ctx'''
        (_, (_, t)) -> throwError $ ExpectedConstructorType t
    ListPat [] -> checkPattern ctx (ConsPat (Free "Lam.Primitive.[]") []) ty
    ListPat subpats -> checkPattern
      ctx
      (foldr (\s acc -> ConsPat (Free "Lam.Primitive.::") [s, acc])
             (ConsPat (Free "Lam.Primitive.[]") [])
             subpats
      )
      ty
    TuplePat subpats ->
      let
        con = case subpats of
          []                       -> error "Type.checkPattern: empty tuple"
          [_] -> error "Type.checkPattern: single-element tuple"
          [_, _]                   -> Free "Lam.Primitive.Tuple2"
          [_, _, _]                -> Free "Lam.Primitive.Tuple3"
          [_, _, _, _]             -> Free "Lam.Primitive.Tuple4"
          [_, _, _, _, _]          -> Free "Lam.Primitive.Tuple5"
          [_, _, _, _, _, _]       -> Free "Lam.Primitive.Tuple6"
          [_, _, _, _, _, _, _]    -> Free "Lam.Primitive.Tuple7"
          [_, _, _, _, _, _, _, _] -> Free "Lam.Primitive.Tuple8"
          _ ->
            error
              $  "Type.checkPattern: cannot (yet) handle tuples of length > 8: "
              <> show subpats
      in  checkPattern ctx (ConsPat con subpats) ty

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

checkCaseAlt :: Type -> Type -> Ctx -> (Pattern, Exp) -> TypeM Ctx
checkCaseAlt expectedAltTy scrutTy ctx (pat, expr) =
  trace' ctx ["checkCaseAlt", debug scrutTy, debug pat, debug expr] $ do
    (inferredAltTy, ctx') <- inferCaseAlt scrutTy ctx (pat, expr)
    subtype ctx' (subst ctx' expectedAltTy) (subst ctx' inferredAltTy)

-- Infer the type of the result when applying a function of type @ty@ to @e@
inferApp :: Ctx -> Type -> Exp -> TypeM (Type, Ctx)
inferApp ctx ty e = trace' ctx ["inferApp", debug ty, debug e] $ case ty of
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
  _ -> throwError $ InfAppFailure ty e

-- Like inferApp but we pass the type of the argument, not the argument itself.
-- We use this when inferring the types of patterns.
inferApp' :: Ctx -> Type -> Type -> TypeM (Ctx, Type)
inferApp' ctx fnTy argTy =
  trace' ctx ["inferApp'", debug fnTy, debug argTy] $ case fnTy of
    Forall u a -> do
      alpha <- newE
      ctx'  <- extendE alpha ctx
      inferApp' ctx' (substEForU alpha u a) argTy
    EType alpha -> do
      a1   <- newE
      a2   <- newE
      ctx' <- extendE a2 ctx >>= extendE a1 >>= extendSolved
        alpha
        (Fn (EType a1) (EType a2))
      ctx'' <- subtype ctx' argTy (EType a1)
      pure (ctx'', EType a2)
    Fn a b -> do
      ctx' <- subtype ctx argTy a
      pure (ctx', b)
    _ -> throwError $ InfAppFailure' fnTy argTy

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
           deriving (Eq, Show)

todoError :: String -> TypeM a
todoError = throwError . TodoError

throwError :: Error -> TypeM a
throwError err = Control.Monad.Except.throwError (LocatedError Nothing err)

-- Tests

prop_uval_infers_unit :: Property
prop_uval_infers_unit = property $ do
  ctx <- (primCtx <>) <$> forAll genCtx
  let expr = Con (Free "Lam.Primitive.Unit")
      ty   = TCon "Lam.Primitive.Unit" []
  fmap fst (runTypeM defaultTypeEnv (infer ctx expr)) === Right ty

prop_checks_bad_unit_annotation :: Property
prop_checks_bad_unit_annotation = property $ do
  ctx <- (primCtx <>) <$> forAll genCtx
  let expr = Con (Free "Lam.Primitive.Unit")
      ty   = Fn unit unit
  runTypeM defaultTypeEnv (check ctx expr ty)
    === Left (LocatedError Nothing (SubtypingFailure unit (Fn unit unit)))

prop_infers_simple_app :: Property
prop_infers_simple_app = property $ do
  let ctx = primCtx
  v <- forAll genV
  let uval = Con (Free "Lam.Primitive.Unit")
  let expr = App (Ann (Lam v uval) (Fn unit unit)) uval
  fmap fst (runTypeM defaultTypeEnv (infer ctx expr)) === Right unit

prop_infers_app_with_context :: Property
prop_infers_app_with_context = property $ do
  -- The context contains id : Unit -> Unit
  let uval = Con (Free "Lam.Primitive.Unit")
      ctx  = [Var (Free "id") (Fn unit unit)] <> primCtx
  -- The expression is id Unit
  let expr = App (VarExp (Free "id")) uval
  -- The inferred type should be Unit
  fmap fst (runTypeM defaultTypeEnv (infer ctx expr)) === Right unit

prop_infers_polymorphic_app :: Property
prop_infers_polymorphic_app = property $ do
  -- The context contains id     : forall a. a -> a
  --                      idUnit : Unit -> Unit
  let uval = Con (Free "Lam.Primitive.Unit")
      a    = U 0 "a"
      ctx =
        [ Var (Free "id")     (Forall a (Fn (UType a) (UType a)))
          , Var (Free "idUnit") (Fn unit unit)
          ]
          <> primCtx
  -- The expression is idUnit (id Unit)
  let expr = App (VarExp (Free "idUnit")) (App (VarExp (Free "id")) uval)
  -- The inferred type should be Unit
  fmap fst (runTypeM defaultTypeEnv (infer ctx expr)) === Right unit

prop_infers_list_app :: Property
prop_infers_list_app = property $ do
  let a    = U 0 "a"
      ty   = Forall a (list (UType a))
      -- [] : forall a. List a
      ctx  = [Var (Free "[]") ty]
      expr = VarExp (Free "[]")
  fmap fst (runTypeM defaultTypeEnv (infer ctx expr)) === Right ty

prop_infers_bool_case :: Property
prop_infers_bool_case = property $ do
  let ctx   = primCtx
      -- case True of
      --   True -> ()
      --   False -> ()
      expr1 = Case
        (Con (Free "Lam.Primitive.True"))
        [ (BoolPat True , Con (Free "Lam.Primitive.Unit"))
        , (BoolPat False, Con (Free "Lam.Primitive.Unit"))
        ]
      -- case True of
      --   True -> True
      --   False -> ()
      expr2 = Case
        (Con (Free "Lam.Primitive.True"))
        [ (BoolPat True , Con (Free "Lam.Primitive.True"))
        , (BoolPat False, Con (Free "Lam.Primitive.Unit"))
        ]
  fmap fst (runTypeM defaultTypeEnv (infer ctx expr1)) === Right unit
  runTypeM defaultTypeEnv (infer ctx expr2)
    === Left (LocatedError Nothing (SubtypingFailure bool unit))

prop_checks_higher_kinded_application :: Property
prop_checks_higher_kinded_application = property $ do
  -- (\x -> x) : f a -> f a should check
  let
    f     = U 1 "f"
    a     = U 2 "a"
    expr1 = Lam (Bound 0) (VarExp (Bound 0))
    type1 = Forall
      f
      (Forall a (Fn (TApp (UType f) [UType a]) (TApp (UType f) [UType a])))
  void (runTypeM defaultTypeEnv (check primCtx expr1 type1)) === Right ()
  -- ((\x -> x) : f a -> f a) [1] : [Int] should check
  -- ((\x -> x) : f a -> f a) [1]         should infer [Int]
  let expr2 = App
        (Ann expr1 type1)
        (App (App (VarExp (Free "Lam.Primitive.::")) (Int 1))
             (VarExp (Free "Lam.Primitive.[]"))
        )
      type2 = list int
  void (runTypeM defaultTypeEnv (check primCtx expr2 type2)) === Right ()
  runTypeM defaultTypeEnv (infer' primCtx expr2) === Right (list int)
  -- ((\x -> x) : f a -> f a) True : [Int] should fail to check
  -- ((\x -> x) : f a -> f a) True         should infer some unknown unification variables
  let expr3 = App (Ann expr1 type1) (VarExp (Free "Lam.Primitive.True"))
      type3 = list int
  void (runTypeM defaultTypeEnv (check primCtx expr3 type3)) === Left
    (LocatedError
      Nothing
      (SubtypingFailure (TCon "Lam.Primitive.Bool" [EType (E 1)]) (list int))
    )
  fmap fst (runTypeM defaultTypeEnv (infer primCtx expr3))
    === Right (TApp (EType (E 0)) [EType (E 1)])

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

-- Used to trace both the input and output of a function
traceWrap :: (Show a, Show b) => a -> TypeM b -> TypeM b
traceWrap x ym = pTrace x $ do
  y <- ym
  pTrace y (pure y)

-- Like trace, but specifically for recursive calls in TypeM
-- If debugging is enabled, prints the given args, indented by the recursion
-- depth (which it increments), and the context.
trace' :: Ctx -> [String] -> TypeM a -> TypeM a
trace' ctx args m = do
  i         <- asks envDepth
  debugging <- asks envDebug
  let indent = replicate i '|'
      msg    = indent <+> debugCtx ctx <+> sepBy " " args
  if debugging then trace msg (local incDepth m) else local incDepth m

(<+>) :: String -> String -> String
a <+> b = a <> " " <> b

sepBy :: String -> [String] -> String
sepBy sep xs = intercalate sep xs
