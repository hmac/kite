-- This module converts a Kite code into Haskell code in pseudo-HOAS style
-- It should allow much faster evaluation.
{-# LANGUAGE RecursiveDo #-}
module Interpret
  ( Value(..)
  , Error(..)
  , interpretAndRun
  , interpretAndRunMain
  , printValue
  ) where


import           Control.Monad                  ( (>=>) )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Control.Monad.Fix              ( MonadFix )
import qualified Data.List.NonEmpty            as NE
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as Map
import           Data.Name                      ( Name(..)
                                                , RawName
                                                )
import           ModuleGroup                    ( TypedModuleGroup(..) )
import           Prettyprinter
import qualified Prim
import           Syn.Print                      ( printRecordSyntax )
import           Syn.Typed
import           Text.Read                      ( readMaybe )
import           Util

data Error = BadListArg String
           | UnknownVariable Name
           | UnknownConstructor Name
           | ApplicationOfNonFunction String
           | RecordMissingField String
           | ProjectOnNonRecord
           | UnconsCharBadArg
           | BadPrim Primitive
           | UnknownPrim RawName
           | BadStringInterpolationArg
           | BadPattern Pattern String
  deriving (Eq, Show)

instance Pretty Error where
  pretty = \case
    BadListArg a -> "Bad list argument:" <+> pretty a
    UnknownVariable v -> "Unknown variable:" <+> pretty v
    UnknownConstructor c -> "Unknown constructor" <+> pretty c
    ApplicationOfNonFunction e -> "Application of non-function:" <+> pretty e
    RecordMissingField f -> "Record missing a required field:" <+> pretty f
    ProjectOnNonRecord -> "Attempted to project a field from a non-record"
    UnconsCharBadArg ->
      "Argument to $uncons expected to be a function, but it was not"
    BadPrim     p -> "Don't know how to apply primitive:" <+> pretty (show p)
    UnknownPrim p -> "Unknown primitive:" <+> pretty (show p)
    BadStringInterpolationArg ->
      "Expression in string interpolation did not evaluate to a string"
    BadPattern p e -> "Bad pattern:" <+> pretty (show p) <+> pretty (show e)

-- The 'm' parameter is the monad in which we're evaluating.
-- It's here so we can construct lambdas ('Abs') which may throw an error when applied to the wrong
-- type of argument.
data Value m = Const Constant
           | Cons Name [Value m]
           | Record (Map String (Value m))
           | Tuple [Value m]
           | List [Value m]
           | Abs (Value m -> m (Value m))
           | FCall String [Value m]
           -- TODO: still needed?
           | Error String

instance Show (Value m) where
  show = show . printValue

data Constant = Int Int
              | String String
              | Char Char
              | Bool Bool
              | Unit
              | Prim Primitive
         deriving (Show, Eq)

printConstant :: Constant -> Doc a
printConstant = \case
  Int    i -> pretty i
  String s -> "\"" <> pretty s <> "\""
  Char   c -> squotes (pretty c)
  Bool   b -> pretty b
  Prim   _ -> "<builtin>"
  Unit     -> "()"

data Primitive = PrimStringAppend
               | PrimStringChars
               | PrimStringConsChar
               | PrimStringUnconsChar
               | PrimAdd
               | PrimSub
               | PrimDiv
               | PrimMult
               | PrimCompose
               | PrimShowInt
               | PrimShowChar
               | PrimEqInt
               | PrimEqChar
               | PrimReadInt
         deriving (Show, Eq)

printValue :: Value m -> Doc a
printValue = \case
  Const c     -> printConstant c
  Cons c args -> pretty c <+> hsep (map printValue args)
  Record r ->
    printRecordSyntax equals $ map (bimap pretty printValue) $ Map.toList r
  Tuple es     -> parens $ hsep $ punctuate comma $ map printValue es
  List  es     -> brackets $ hsep $ punctuate comma $ map printValue es
  Abs   _      -> "<function>"
  FCall s args -> pretty s <+> hsep (map printValue args)
  Error s      -> "Error: " <> pretty s


type Env m = Map Name (Value m)

defaultEnv :: MonadError Error m => Env m
defaultEnv = Map.fromList
  [ (TopLevel Prim.name "[]", List [])
  , ( TopLevel Prim.name "::"
    , Abs
      (\x -> pure $ Abs
        (\case
          List xs' -> pure $ List (x : xs')
          e        -> throwError $ BadListArg (show e)
        )
      )
    )
  ]

interpretAndRunMain
  :: (MonadFix m, MonadError Error m) => TypedModuleGroup -> m (Value m)
interpretAndRunMain g@(TypedModuleGroup m _) =
  interpretAndRun (TopLevel (moduleName m) "main") g

interpretAndRun
  :: (MonadFix m, MonadError Error m) => Name -> TypedModuleGroup -> m (Value m)
interpretAndRun mainFunc (TypedModuleGroup m deps) = do
  env <- foldM interpretModule defaultEnv deps
  m'  <- interpretModule env m
  case Map.lookup mainFunc m' of
    Just v  -> pure v
    Nothing -> pure $ Error $ "Unknown variable " <> show mainFunc

interpretModule
  :: (MonadFix m, MonadError Error m) => Env m -> Module -> m (Env m)
interpretModule env Module { moduleDecls = decls } =
  -- To ensure the right things are in scope when needed, we process decls in a
  -- particular order:
  -- 1. data decls
  -- 2. everything else
  let ordering = \case
        (DataDecl _) -> 0 :: Int
        (FunDecl  _) -> 1
      orderedDecls = sortOn ordering decls
  in  foldM
        (\env_ decl ->
          foldl' (\e (k, v) -> Map.insert k v e) env_
            <$> interpretDecl env_ decl
        )
        env
        orderedDecls

interpretDecl
  :: (MonadFix m, MonadError Error m) => Env m -> Decl -> m [(Name, Value m)]
interpretDecl env = \case
  FunDecl f -> do
    f' <- interpretFun env f
    pure [f']
  DataDecl d -> pure $ interpretData d

-- When interpreting a function, we insert its own name and value into the environment so that it
-- can refer to itself.
interpretFun
  :: (MonadFix m, MonadError Error m) => Env m -> Fun -> m (Name, Value m)
interpretFun env Fun { funName = name, funExpr = expr } = mdo
  fun <- interpretExpr (Map.insert name fun env) expr
  pure (name, fun)

interpretData :: Monad m => Data -> [(Name, Value m)]
interpretData Data { dataCons = datacons } = map interpretDatacon datacons
 where
  interpretDatacon DataCon { conName = name, conMeta = meta } =
    (name, go (replicate (conMetaArity meta) ()) [])
   where
    -- TODO: this should just be a fold
    go []       collectedArgs = Cons name (reverse collectedArgs)
    go (_ : as) collected     = Abs (\v -> pure (go as (v : collected)))

interpretExpr :: MonadError Error m => Env m -> Exp -> m (Value m)
interpretExpr env expr_ = case expr_ of
  VarT _ (TopLevel m n) | m == Prim.name -> interpretPrim n
  VarT _ n                               -> case Map.lookup n env of
    Just v  -> pure v
    Nothing -> throwError $ UnknownVariable n
  AppT _ a b -> interpretExpr env a >>= \case
    Abs f -> interpretExpr env b >>= f
    e     -> throwError $ ApplicationOfNonFunction (show e)
  IAppT _ a b -> interpretExpr env a >>= \case
    Abs f -> interpretExpr env b >>= f
    e     -> throwError $ ApplicationOfNonFunction (show e)
  ConT _ c _
    | c == TopLevel Prim.name "[]" -> pure $ List []
    | c == TopLevel Prim.name "::" -> pure $ Abs
      (\x -> pure
        (Abs
          (\case
            List xs' -> pure $ List (x : xs')
            e        -> throwError $ BadListArg (show e)
          )
        )
      )
    | otherwise -> lookupCon c env
  IAbsT _ pat _ e -> pure $ Abs $ \v -> do -- match v against the pattern
    applyPattern env v pat >>= \case
      Nothing   -> pure $ Error "pattern match failed"
      Just env' -> interpretExpr env' e
  LetT _ binds expr -> do
    env' <- foldM
      (\env_ (n, e, _) -> do
        e' <- interpretExpr env_ e
        pure $ Map.insert n e' env_
      )
      env
      binds
    interpretExpr env' expr
  CaseT _ scrut alts -> do
    scrut' <- interpretExpr env scrut
    interpretCase env scrut' alts
  MCaseT _ alts ->
    interpretMCase $ fmap (\(pats, rhs) -> (pats, env, rhs)) alts
  AnnT _ e _                   -> interpretExpr env e
  HoleT t n -> pure $ Error $ "Found hole " <> show n <> " : " <> show t
  IntLitT _ i                  -> pure $ Const (Int i)
  UnitLitT _                   -> pure $ Const Unit
  TupleLitT _ args             -> Tuple <$> mapM (interpretExpr env) args
  ListLitT  _ args             -> List <$> mapM (interpretExpr env) args
  StringInterpT _ prefix comps -> interpretStringInterp env prefix comps
  StringLitT _ s               -> pure $ Const (String s)
  CharLitT   _ c               -> pure $ Const (Char c)
  BoolLitT   _ b               -> pure $ Const (Bool b)
  RecordT _ fields ->
    Record . Map.fromList <$> mapM (secondM (interpretExpr env)) fields
  ProjectT _ e field -> interpretExpr env e >>= \case
    Record fields -> case Map.lookup field fields of
      Just val -> pure val
      Nothing  -> throwError $ RecordMissingField field
    _ -> throwError ProjectOnNonRecord
  FCallT _ s args -> FCall s <$> mapM (interpretExpr env) args
  ImplicitT t (Solved v) -> interpretExpr env $ VarT t v
  ImplicitT t Unsolved -> pure $ Error $ "Found unsolved implicit : " <> show t

interpretPrim :: MonadError Error m => RawName -> m (Value m)
interpretPrim = \case
  "+"            -> binaryPrim PrimAdd
  "*"            -> binaryPrim PrimMult
  "-"            -> binaryPrim PrimSub
  "/"            -> binaryPrim PrimDiv
  "."            -> binaryPrim PrimCompose
  "appendString" -> binaryPrim PrimStringAppend
  "$chars"       -> unaryPrim PrimStringChars
  "$consChar"    -> binaryPrim PrimStringConsChar
  "$unconsChar"  -> ternaryPrim PrimStringUnconsChar
  "$showInt"     -> unaryPrim PrimShowInt
  "$showChar"    -> unaryPrim PrimShowChar
  "$eqInt"       -> binaryPrim PrimEqInt
  "$readInt"     -> ternaryPrim PrimReadInt
  "$eqChar"      -> binaryPrim PrimEqChar
  p              -> throwError $ UnknownPrim p
 where
  unaryPrim p = pure $ Abs (\x -> applyPrim p [x])
  binaryPrim p = pure $ Abs (\x -> pure (Abs (\y -> applyPrim p [x, y])))
  ternaryPrim p =
    pure $ Abs
      (\x -> pure $ Abs (\y -> pure $ Abs (\z -> applyPrim p [x, y, z])))

applyPrim :: MonadError Error m => Primitive -> [Value m] -> m (Value m)
applyPrim PrimAdd [Const (Int x), Const (Int y)] = pure $ Const $ Int $ x + y
applyPrim PrimMult [Const (Int x), Const (Int y)] = pure $ Const $ Int $ x * y
applyPrim PrimSub [Const (Int x), Const (Int y)] = pure $ Const $ Int $ x - y
applyPrim PrimDiv [Const (Int x), Const (Int y)] = pure $ Const $ Int $ div x y
applyPrim PrimCompose [Abs g, Abs f] = pure $ Abs $ f >=> g
applyPrim PrimStringAppend [Const (String x), Const (String y)] =
  pure $ Const $ String $ x <> y
applyPrim PrimStringChars [Const (String s)] =
  pure $ List $ map (Const . Char) s
applyPrim PrimStringConsChar [Const (Char c), Const (String s)] =
  pure $ Const $ String (c : s)
applyPrim PrimStringUnconsChar [Const (String s), def, Abs f] = case s of
  ""       -> pure def
  (c : cs) -> f (Const (Char c)) >>= \case
    Abs f' -> f' (Const (String cs))
    _      -> throwError UnconsCharBadArg
applyPrim PrimShowInt  [Const (Int  x)] = pure $ Const $ String $ show x
applyPrim PrimShowChar [Const (Char c)] = pure $ Const $ String [c]
applyPrim PrimEqInt [Const (Int x), Const (Int y)] =
  pure $ Const $ Bool $ x == y
applyPrim PrimEqChar [Const (Char x), Const (Char y)] =
  pure $ Const $ Bool $ x == y
applyPrim PrimReadInt [Const (String s), def, Abs f] = case readMaybe s of
  Just i  -> f (Const (Int i))
  Nothing -> pure def
applyPrim p _ = throwError $ BadPrim p

interpretCase
  :: MonadError Error m => Env m -> Value m -> [(Pattern, Exp)] -> m (Value m)
interpretCase _ _ [] = pure $ Error "pattern match failed"
interpretCase env scrut ((pat, rhs) : pats) =
  applyPattern env scrut pat >>= \case
    Just env' -> interpretExpr env' rhs
    Nothing   -> interpretCase env scrut pats

-- mcase is a bit tricky because we have multiple arguments to pattern match on.
-- We handle this by pattern matching on each argument as we receive it,
-- whittling down the possible branches as we go.
-- We end when the first branch runs out of patterns or when we run out of
-- matching branches.
interpretMCase
  :: MonadError Error m => NonEmpty ([Pattern], Env m, Exp) -> m (Value m)
interpretMCase (([], env, rhs) :| _) = interpretExpr env rhs
interpretMCase alts                  = pure $ Abs $ \v -> do
  -- match v against the first pattern in each alt, and keep the alts that succeed
  alts' <- traverse
    (\(pats, env, rhs) -> case pats of
      (p : ps) -> fmap (ps, , rhs) <$> applyPattern env v p
      _        -> pure Nothing
    )
    alts
  case sequence alts' of
    Nothing           -> pure $ Error "pattern match failed"
    Just matchingAlts -> interpretMCase matchingAlts

applyPattern
  :: MonadError Error m => Env m -> Value m -> Pattern -> m (Maybe (Env m))
applyPattern env val pattern = case (pattern, val) of
  (VarPat _ n, _) -> pure $ Just $ Map.insert n val env
  (WildPat _ , _) -> pure $ Just env
  (CharPat _ c, Const (Char c')) | c == c'   -> pure $ Just env
                                 | otherwise -> pure Nothing
  (IntPat _ i, Const (Int i')) | i == i'   -> pure $ Just env
                               | otherwise -> pure Nothing
  (BoolPat _ b, Const (Bool b')) | b == b'   -> pure $ Just env
                                 | otherwise -> pure Nothing
  (StringPat _ s, Const (String s')) | s == s'   -> pure $ Just env
                                     | otherwise -> pure Nothing
  (UnitPat _, Const Unit) -> pure $ Just env
  (TuplePat _ pats, Tuple args)
    | length pats == length args -> foldM
      (\env_ (arg, pat) -> case env_ of
        Just e  -> applyPattern e arg pat
        Nothing -> pure Nothing
      )
      (Just env)
      (zip args pats)
    | otherwise -> pure Nothing
  (ListPat _ pats, List args)
    | length pats == length args -> foldM
      (\env_ (arg, pat) -> case env_ of
        Just e  -> applyPattern e arg pat
        Nothing -> pure Nothing
      )
      (Just env)
      (zip args pats)
    | otherwise -> pure Nothing
  (ConsPat _ c _meta pats, Cons c' args)
    | c == c' -> foldM
      (\env_ (arg, pat) -> case env_ of
        Just e  -> applyPattern e arg pat
        Nothing -> pure Nothing
      )
      (Just env)
      (zip args pats)
    | otherwise -> pure Nothing
  (ConsPat _ (TopLevel m "::") _meta pats, List elems) | m == Prim.name ->
    case (elems, pats) of
      (e : es, [p1, p2]) -> do
        env' <- applyPattern env e p1
        case env' of
          Nothing    -> pure Nothing
          Just env'' -> applyPattern env'' (List es) p2
      _ -> throwError $ BadPattern pattern (show val)
  -- otherwise, the pattern can't be matched to the argument
  -- this means a type error has slipped through
  _ -> throwError $ BadPattern pattern (show val)

interpretStringInterp
  :: MonadError Error m
  => Env m
  -> String
  -> NonEmpty (Exp, String)
  -> m (Value m)
interpretStringInterp env prefix comps = Const . String <$> go
  (NE.toList comps)
 where
  go []                     = pure prefix
  go ((e, suffix) : comps') = interpretExpr env e >>= \case
    Const (String s) -> do
      rest <- go comps'
      pure (prefix <> s <> suffix <> rest)
    _ -> throwError BadStringInterpolationArg


lookupCon :: MonadError Error m => Name -> Env m -> m (Value m)
lookupCon n env = case Map.lookup n env of
  Just v -> pure v
  _      -> throwError $ UnknownConstructor n
