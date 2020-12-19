-- This module converts a Kite code into Haskell code in pseudo-HOAS style
-- It should allow much faster evaluation.
module Interpret
  ( Value(..)
  , interpretAndRun
  , interpretAndRunMain
  , printValue
  )
where


import           Util
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as Map
import           Syn.Typed
import           ELC                            ( Constant(..)
                                                , Primitive(..)
                                                )
import           ELC.Primitive                  ( modPrim )
import           Data.Name                      ( Name(..)
                                                , RawName
                                                )
import           ModuleGroup                    ( TypedModuleGroup(..) )
import           Data.Text.Prettyprint.Doc
import           LC.Print                       ( printConstant
                                                , printName
                                                )
import           Syn.Print                      ( printRecordSyntax )

data Value = Const Constant
           | Cons Name [Value]
           | Record (Map String Value)
           | Tuple [Value]
           | List [Value]
           | Abs (Value -> Value)
           | FCall String [Value]
           | Error String

instance Show Value where
  show = show . printValue

printValue :: Value -> Doc a
printValue = \case
  Const c     -> printConstant c
  Cons c args -> printName c <+> hsep (map printValue args)
  Record r ->
    printRecordSyntax equals $ map (bimap pretty printValue) $ Map.toList r
  Tuple es     -> parens $ hsep $ punctuate comma $ map printValue es
  List  es     -> brackets $ hsep $ punctuate comma $ map printValue es
  Abs   _      -> "<function>"
  FCall s args -> pretty s <+> hsep (map printValue args)
  Error s      -> "Error: " <> pretty s


type Env = Map Name Value

defaultEnv :: Env
defaultEnv = Map.fromList
  [ (TopLevel modPrim "[]", List [])
  , ( TopLevel modPrim "::"
    , Abs
      (\x -> Abs
        (\xs -> case xs of
          List xs' -> List (x : xs')
          e        -> error $ "Bad argument to list constructor:" <> show e
        )
      )
    )
  ]

interpretAndRunMain :: TypedModuleGroup -> Value
interpretAndRunMain g@(TypedModuleGroup m _) =
  interpretAndRun (TopLevel (moduleName m) "main") g

interpretAndRun :: Name -> TypedModuleGroup -> Value
interpretAndRun mainFunc (TypedModuleGroup m deps) =
  let env = foldl' interpretModule defaultEnv deps
  in  case Map.lookup mainFunc (interpretModule env m) of
        Just v  -> v
        Nothing -> Error $ "Unknown variable " <> show mainFunc

interpretModule :: Env -> Module -> Env
interpretModule env Module { moduleDecls = decls } =
  -- To ensure the right things are in scope when needed, we process decls in a
  -- particular order:
  -- 1. data decls
  -- 2. everything else
  let ordering = \case
        (DataDecl _) -> 0 :: Int
        (FunDecl  _) -> 1
      orderedDecls = sortOn ordering decls
  in  foldl'
        (\env_ decl -> foldl' (\e (k, v) -> Map.insert k v e)
                              env_
                              (interpretDecl env_ decl)
        )
        env
        orderedDecls

interpretDecl :: Env -> Decl -> [(Name, Value)]
interpretDecl env = \case
  FunDecl  f -> [interpretFun env f]
  DataDecl d -> interpretData d

interpretFun :: Env -> Fun -> (Name, Value)
interpretFun env Fun { funName = name, funExpr = expr } =
  let fun = interpretExpr (Map.insert name fun env) expr in (name, fun)

interpretData :: Data -> [(Name, Value)]
interpretData Data { dataCons = datacons } = map interpretDatacon datacons
 where
  interpretDatacon :: DataCon -> (Name, Value)
  interpretDatacon DataCon { conName = name, conArgs = args } =
    (name, go args [])
   where
    go []       collectedArgs = Cons name (reverse collectedArgs)
    go (_ : as) collected     = Abs (\v -> go as (v : collected))

interpretExpr :: Env -> Exp -> Value
interpretExpr env expr_ = case expr_ of
  VarT (TopLevel m n) _ | m == modPrim -> interpretPrim n
  VarT n _                             -> case Map.lookup n env of
    Just v  -> v
    Nothing -> error $ "Interpret.interpretExpr: unknown variable " <> show n
  AppT a b _ -> case interpretExpr env a of
    Abs f -> f (interpretExpr env b)
    _     -> error $ "Interpret.interpretExpr: non-function in application head"
  ConT c
    | c == TopLevel modPrim "[]" -> List []
    | c == TopLevel modPrim "::" -> Abs
      (\x ->
        (Abs
          (\xs -> case xs of
            List xs' -> List (x : xs')
            _ -> error $ "Interpret.interpretExpr: listCons: non-list argument"
          )
        )
      )
    | otherwise -> lookupCon c env
  AbsT vars e _ -> interpretAbs env (map fst vars) e
  LetT binds expr _ ->
    let env' = foldl'
          (\env_ (n, e, _) -> Map.insert n (interpretExpr env_ e) env_)
          env
          binds
    in  interpretExpr env' expr
  CaseT scrut alts _         -> interpretCase env (interpretExpr env scrut) alts
  MCaseT alts _ -> interpretMCase (map (\(pats, rhs) -> (pats, env, rhs)) alts)
  AnnT   e    _              -> interpretExpr env e
  HoleT n t -> Error $ "Found hole " <> show n <> " : " <> show t
  IntLitT i                  -> Const (Int i)
  UnitLitT                   -> Const Unit
  TupleLitT     args   _     -> Tuple $ map (interpretExpr env) args
  ListLitT      args   _     -> List $ map (interpretExpr env) args
  StringInterpT prefix comps -> interpretStringInterp env prefix comps
  StringLitT s               -> Const (String s)
  CharLitT   c               -> Const (Char c)
  BoolLitT   b               -> Const (Bool b)
  RecordT fields _ -> Record $ Map.fromList $ mapSnd (interpretExpr env) fields
  ProjectT e field _         -> case interpretExpr env e of
    Record fields -> case Map.lookup field fields of
      Just val -> val
      Nothing ->
        error
          $  "Interpret.interpretExpr: field "
          <> show field
          <> " not present in record"
    _ ->
      error $ "Interpret.interpretExpr: cannot project field from non-record"
  FCallT s args _ -> FCall s $ map (interpretExpr env) args

interpretPrim :: RawName -> Value
interpretPrim = \case
  "+"            -> binaryPrim PrimAdd
  "*"            -> binaryPrim PrimMult
  "-"            -> binaryPrim PrimSub
  "appendString" -> binaryPrim PrimStringAppend
  "$chars"       -> unaryPrim PrimStringChars
  "$consChar"    -> binaryPrim PrimStringConsChar
  "$unconsChar"  -> ternaryPrim PrimStringUnconsChar
  "$showInt"     -> unaryPrim PrimShowInt
  "$showChar"    -> unaryPrim PrimShowChar
  "$eqInt"       -> binaryPrim PrimEqInt
  "$eqChar"      -> binaryPrim PrimEqChar
  p -> error $ "Interpret.interpretPrim: unknown primitive " <> show p
 where
  unaryPrim p = Abs (\x -> applyPrim p [x])
  binaryPrim p = Abs (\x -> (Abs (\y -> applyPrim p [x, y])))
  ternaryPrim p = Abs (\x -> Abs (\y -> Abs (\z -> applyPrim p [x, y, z])))

applyPrim :: Primitive -> [Value] -> Value
applyPrim PrimAdd  [Const (Int x), Const (Int y)] = Const $ Int $ x + y
applyPrim PrimMult [Const (Int x), Const (Int y)] = Const $ Int $ x * y
applyPrim PrimSub  [Const (Int x), Const (Int y)] = Const $ Int $ x - y
applyPrim PrimStringAppend [Const (String x), Const (String y)] =
  Const $ String $ x <> y
applyPrim PrimStringChars [Const (String s)] = List $ map (Const . Char) s
applyPrim PrimStringConsChar [Const (Char c), Const (String s)] =
  Const $ String (c : s)
applyPrim PrimStringUnconsChar [Const (String s), def, Abs f] = case s of
  ""       -> def
  (c : cs) -> case f (Const (Char c)) of
    Abs f' -> f' (Const (String cs))
    _      -> error $ "Interpret.applyPrim: unconsChar: bad argument"
applyPrim PrimShowInt [Const (Int x)] = Const $ String $ show x
applyPrim PrimShowChar [Const (Char c)]                 = Const $ String $ [c]
applyPrim PrimEqInt    [Const (Int  x), Const (Int y) ] = Const $ Bool $ x == y
applyPrim PrimEqChar   [Const (Char x), Const (Char y)] = Const $ Bool $ x == y
applyPrim p _ =
  error $ "Interpret.applyPrim: bad application of primitive " <> show p

interpretAbs :: Env -> [Name] -> Exp -> Value
interpretAbs env [] body = interpretExpr env body
interpretAbs env (v : vars) body =
  Abs (\arg -> interpretAbs (Map.insert v arg env) vars body)

interpretCase :: Env -> Value -> [(Pattern, Exp)] -> Value
interpretCase _ _ [] = Error "pattern match failed"
interpretCase env scrut ((pat, rhs) : pats) =
  case applyPattern env scrut pat of
    Just env' -> interpretExpr env' rhs
    Nothing   -> interpretCase env scrut pats

-- mcase is a bit tricky because we have multiple arguments to pattern match on.
-- We handle this by pattern matching on each argument as we receive it,
-- whittling down the possible branches as we go.
-- We end when the first branch runs out of patterns or when we run out of
-- matching branches.
interpretMCase :: [([Pattern], Env, Exp)] -> Value
interpretMCase []                   = Error "pattern match failed"
interpretMCase (([], env, rhs) : _) = interpretExpr env rhs
interpretMCase alts                 = Abs $ \v ->
  -- match v against the first pattern in each alt, and keep the alts that succeed
  let alts' = mapMaybe
        (\(pats, env, rhs) -> case pats of
          (p : ps) -> (ps, , rhs) <$> applyPattern env v p
          _        -> Nothing
        )
        alts
  in  interpretMCase alts'

applyPattern :: Env -> Value -> Pattern -> Maybe Env
applyPattern env val pattern = case (pattern, val) of
  (VarPat n, _) -> Just $ Map.insert n val env
  (WildPat , _) -> Just env
  (CharPat c, Const (Char c')) | c == c'   -> Just env
                               | otherwise -> Nothing
  (IntPat i, Const (Int i')) | i == i'   -> Just env
                             | otherwise -> Nothing
  (BoolPat b, Const (Bool b')) | b == b'   -> Just env
                               | otherwise -> Nothing
  (StringPat s, Const (String s')) | s == s'   -> Just env
                                   | otherwise -> Nothing
  (UnitPat, Const Unit) -> Just env
  (TuplePat pats, Tuple args)
    | length pats == length args -> foldM
      (\env_ (arg, pat) -> applyPattern env_ arg pat)
      env
      (zip args pats)
    | otherwise -> Nothing
  (ListPat pats, List args)
    | length pats == length args -> foldM
      (\env_ (arg, pat) -> applyPattern env_ arg pat)
      env
      (zip args pats)
    | otherwise -> Nothing
  (ConsPat c pats, Cons c' args)
    | c == c' -> foldM (\env_ (arg, pat) -> applyPattern env_ arg pat)
                       env
                       (zip args pats)
    | otherwise -> Nothing
  (ConsPat (TopLevel m "::") pats, List elems) | m == modPrim ->
    case (elems, pats) of
      ((e : es), [p1, p2]) -> do
        env' <- applyPattern env e p1
        applyPattern env' (List es) p2
      _ ->
        error
          $  "Interpret.applyPattern: bad pattern: "
          <> show pattern
          <> " "
          <> show val
  -- otherwise, the pattern can't be matched to the argument
  -- this means a type error has slipped through
  _ ->
    error
      $  "Interpret.applyPattern: bad pattern: "
      <> show pattern
      <> " "
      <> show val

interpretStringInterp :: Env -> String -> [(Exp, String)] -> Value
interpretStringInterp _ s [] = Const (String s)
interpretStringInterp env prefix ((e, suffix) : comps) =
  case interpretExpr env e of
    Const (String s) -> interpretStringInterp env (prefix <> s <> suffix) comps
    _ ->
      error $ "Interpret.interpretStringInterp: cannot interpolate non-string"


lookupCon :: Name -> Env -> Value
lookupCon n env = case Map.lookup n env of
  Just v -> v
  _      -> error $ "unknown constructor: " <> show n
