module ELC.Compile
  ( collapseEnv
  , Env
  , defaultEnv
  , translateModule
  )
where

-- Compile T.Module to ELC.Exp

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Util
import           Control.Monad.State.Strict
import           Data.List.Extra                ( groupOn )
import           Data.Foldable                  ( foldlM
                                                , foldrM
                                                )

import           ELC
import           ELC.Primitive
import           Data.Name
import           NameGen                        ( NameGen
                                                , freshM
                                                )
import qualified Syn.Typed                     as T

-- defs are normal definitions
newtype Env = Env { envDefs :: Map Name Exp }
  deriving Show

defaultEnv :: Env
defaultEnv = Env { envDefs = Map.fromList primConstructors }

merge :: Env -> [(Name, Exp)] -> Env
merge env newDefs = env { envDefs = envDefs env <> Map.fromList newDefs }

collapseEnv :: Env -> [(Name, Exp)]
collapseEnv = Map.toList . envDefs

lookupDef :: Name -> Env -> Maybe Exp
lookupDef n env = Map.lookup n (envDefs env)

fresh :: NameGen Name
fresh = freshM (\i -> Local (Name ("$elc" ++ show i)))

-- TODO: remove unreachable definitions
translateModule :: Env -> T.Module -> NameGen Env
translateModule env T.Module { T.moduleDecls = decls } =
  -- To ensure the right things are in scope when needed, we process decls in a
  -- particular order:
  -- 1. data decls
  -- 4. everything else
  let ordering :: T.Decl -> Int
      ordering = \case
        (T.DataDecl _) -> 0
        (T.FunDecl  _) -> 3
      orderedDecls = sortOn ordering decls
  in  foldlM (\e decl -> merge e <$> translateDecl e decl) env orderedDecls

translateDecl :: Env -> T.Decl -> NameGen [(Name, Exp)]
translateDecl env (T.FunDecl T.Fun { T.funName = n, T.funDefs = defs }) = do
  let numVars = length (T.defArgs (head defs))
  varNames <- replicateM numVars fresh
  let vars = map VarPat varNames
  equations <- mapM (translateDef env) defs
  caseExpr  <- match varNames equations (Bottom "pattern match failed")
  pure [(n, buildAbs caseExpr vars)]

translateDecl _env (T.DataDecl d) = do
  let cons = T.dataCons d
  if length cons > 1
    then
      let cs = zipWith (translateSumCon cs) [0 ..] cons
      in  pure $ map (\c -> (conName c, Cons c [])) cs
    else concat <$> mapM translateProdCon cons

-- Translate a function definition into a form understood by the pattern match
-- compiler.
translateDef :: Env -> T.Def -> NameGen Equation
translateDef env def = do
  args <- mapM (translatePattern env) (T.defArgs def)
  expr <- translateExpr env (T.defExpr def)
  pure (args, expr)

-- Note: we do a weird trick here where each constructor has a reference to a
-- list of all the constructors for its type, which includes itself.
-- This is so that the pattern match compiler can perform exhaustiveness
-- checking on patterns that use the constructor.
translateSumCon :: [Con] -> Int -> T.DataCon -> Con
translateSumCon f t T.DataCon { T.conName = n, T.conArgs = args } =
  Sum { conName = n, conArity = length args, sumTag = t, sumFamily = f }
translateSumCon _ _ T.RecordCon{} =
  error "Cannot translate record constructors in sums yet"

translateProdCon :: T.DataCon -> NameGen [(Name, Exp)]
translateProdCon T.DataCon { T.conName = n, T.conArgs = args } =
  pure [(n, Cons Prod { conName = n, conArity = length args } [])]
translateProdCon T.RecordCon { T.conName = n, T.conFields = fields } = do
  let constructor = Prod { conName = n, conArity = length fields }
      wildPat     = VarPat <$> fresh
      selectorPat i var = mapM
        (\x -> if x == i then pure (VarPat var) else wildPat)
        [0 .. length fields - 1]
  selectors <- zipWithM
    (\i (fieldName, _) -> do
      var    <- fresh
      selPat <- selectorPat i var
      pure (fieldName, Abs (ConPat constructor selPat) (Var var))
    )
    [0 ..]
    fields
  pure $ (n, Cons constructor []) : selectors

translatePattern :: Env -> T.Pattern -> NameGen Pattern
translatePattern _   (T.VarPat    n ) = pure (VarPat n)
translatePattern _   (T.IntPat    i ) = pure (ConstPat (Int i))
translatePattern _   (T.BoolPat   b ) = pure (ConstPat (Bool b))
translatePattern _   (T.StringPat s ) = pure (ConstPat (String s))
translatePattern env (T.ListPat   es) = do
  pats <- mapM (translatePattern env) es
  pure (buildListPat pats)
translatePattern env (T.TuplePat es) = do
  pats <- mapM (translatePattern env) es
  pure (buildTuplePat pats)
translatePattern env (T.ConsPat n pats) = do
  pats' <- mapM (translatePattern env) pats
  pure $ ConPat (lookupCon n env) pats'
translatePattern _ T.WildPat = VarPat <$> fresh

lookupCon :: Name -> Env -> Con
lookupCon n env = case lookupDef n env of
  Just (Cons c _) -> c
  _               -> error $ "unknown constructor: " <> show n

buildListPat :: [Pattern] -> Pattern
buildListPat []       = ConPat listNil []
buildListPat (x : xs) = ConPat listCons [x, buildListPat xs]

buildTuplePat :: [Pattern] -> Pattern
buildTuplePat elems = case length elems of
  2 -> ConPat tuple2 elems
  3 -> ConPat tuple3 elems
  4 -> ConPat tuple4 elems
  5 -> ConPat tuple5 elems
  6 -> ConPat tuple6 elems
  n -> error $ "cannot handle tuples of length " <> show n

translateExpr :: Env -> T.Exp -> NameGen Exp
translateExpr _   (T.IntLitT  i _        ) = pure (Const (Int i) [])
translateExpr _   (T.BoolLitT b _        ) = pure (Const (Bool b) [])
translateExpr env (T.StringLitT s parts _) = translateStringLit env s parts
translateExpr env (T.ListLitT elems _    ) = do
  elems' <- mapM (translateExpr env) elems
  buildList elems'
translateExpr env (T.TupleLitT elems _) = do
  elems' <- mapM (translateExpr env) elems
  pure (buildTuple elems')

-- TODO: what's a better way to handle this?
-- possible have a Prelude module which puts these variables in scope, bound to
-- "$prim$Num$add" or something?
translateExpr _ (T.VarT (TopLevel m "+") _) | m == modPrim = binaryPrim PrimAdd
translateExpr _ (T.VarT (TopLevel m "*") _) | m == modPrim = binaryPrim PrimMult
translateExpr _ (T.VarT (TopLevel m "-") _) | m == modPrim = binaryPrim PrimSub
translateExpr _ (T.VarT (TopLevel m "appendString") _) | m == modPrim =
  binaryPrim PrimStringAppend
translateExpr _ (T.VarT (TopLevel m "$showInt") _) | m == modPrim = do
  v <- fresh
  pure $ Abs (VarPat v) (Const (Prim PrimShowInt) [Var v])
translateExpr _ (T.VarT (TopLevel m "show") _) | m == modPrim = do
  v <- fresh
  pure $ Abs (VarPat v) (Const (Prim PrimShow) [Var v])
translateExpr _ (T.VarT (TopLevel m "$eqInt") _) | m == modPrim = do
  v1 <- fresh
  v2 <- fresh
  pure $ Abs (VarPat v1)
             (Abs (VarPat v2) (Const (Prim PrimEqInt) [Var v1, Var v2]))

translateExpr _   (T.VarT n _) = pure (Var n)
translateExpr env (T.AppT a b) = do
  a' <- translateExpr env a
  b' <- translateExpr env b
  pure $ App a' b'

-- We translate a constructor into a series of nested lambda abstractions, one
-- for each argument to the constructor. When applied, the result is a fully
-- saturated constructor.
translateExpr env (T.ConT n) = do
  let con = lookupCon n env
      a   = conArity con
  newVars <- replicateM a fresh
  pure $ buildAbs (Cons con (map Var newVars)) (map VarPat newVars)
translateExpr _ (T.HoleT n _) = pure $ Bottom ("Hole encountered: " <> show n)
translateExpr env (T.AbsT vars e) = do
  body <- translateExpr env e
  pure $ buildAbs body (map (VarPat . fst) vars)
translateExpr env (T.LetT alts expr _) = do
  alts' <- mapM
    (\(n, e) -> do
      e' <- translateExpr env e
      pure (VarPat n, e')
    )
    alts
  expr' <- translateExpr env expr
  pure $ LetRec alts' expr'
translateExpr _   T.LetAT{}              = error "Cannot translate LetA yet"
-- case (foo bar) of
--   p1 -> e1
--   p2 -> e2
-- ==>
-- let $v = foo bar
--  in ((\p1 -> e1) $v) [] ((\p2 -> e2) $v) [] BOTTOM
-- ((\p1 -> e1) [] (\p2 -> e2)) (foo bar)
translateExpr env (T.CaseT scrut alts _) = do
  var    <- fresh
  scrut' <- translateExpr env scrut
  alts'  <- mapM
    (\(T.AltT p e) -> do
      p' <- translatePattern env p
      e' <- translateExpr env e
      pure $ App (Abs p' e') (Var var)
    )
    alts
  let lams = foldr Fatbar (Bottom "pattern match failure") alts'
  pure $ Let (VarPat var) scrut' lams
translateExpr env (T.RecordT fields _type) = translateRecord env fields
translateExpr env (T.ProjectT e l _type  ) = translateRecordProjection env e l

-- "hi #{name}!" ==> "hi " <> name <> "!"
-- The typechecker ensures that name : String
translateStringLit :: Env -> String -> [(T.Exp, String)] -> NameGen Exp
translateStringLit _   prefix []    = pure $ Const (String prefix) []
translateStringLit env prefix parts = do
  rest <- go parts
  foldrM
    (\x acc -> do
      f <- stringAppendFn
      pure $ App (App f x) acc
    )
    (Const (String "") [])
    (Const (String prefix) [] : rest)
 where
  stringAppendFn = do
    l      <- fresh
    r      <- fresh
    append <- binaryPrim PrimStringAppend
    pure $ Abs (VarPat l) (Abs (VarPat r) (App (App append (Var l)) (Var r)))
  go :: [(T.Exp, String)] -> NameGen [Exp]
  go []            = pure []
  go ((e, s) : is) = do
    e'   <- translateExpr env e
    rest <- go is
    pure $ e' : Const (String s) [] : rest

-- here we follow the same scheme as with normal constructors
buildList :: [Exp] -> NameGen Exp
buildList = flip foldrM (Cons listNil []) $ \e acc -> do
  lvar <- fresh
  rvar <- fresh
  pure $ App
    (App
      (Abs (VarPat lvar)
           (Abs (VarPat rvar) (Cons listCons [Var lvar, Var rvar]))
      )
      e
    )
    acc

-- TODO: fix as above
buildTuple :: [Exp] -> Exp
buildTuple elems = case length elems of
  2 -> Cons tuple2 elems
  3 -> Cons tuple3 elems
  4 -> Cons tuple4 elems
  5 -> Cons tuple5 elems
  6 -> Cons tuple6 elems
  n -> error $ "cannot handle tuples of length " <> show n

translateRecord :: Env -> [(Name, T.Exp)] -> NameGen Exp
translateRecord env fields = do
  fields' <- mapM (bimapM (pure . unName . toRaw) (translateExpr env)) fields
  (pure . Record . Map.fromList) fields'
  where unName (Name n) = n

translateRecordProjection :: Env -> T.Exp -> Name -> NameGen Exp
translateRecordProjection env expr label =
  RecordProject <$> translateExpr env expr <*> pure (unName (toRaw label))
  where unName (Name n) = n

binaryPrim :: Primitive -> NameGen Exp
binaryPrim p = do
  v1 <- fresh
  v2 <- fresh
  pure $ Abs (VarPat v1) (Abs (VarPat v2) (Const (Prim p) [Var v1, Var v2]))

subst :: Exp -> Name -> Exp -> Exp
subst a n (Const c es) = Const c (map (subst a n) es)
subst a n (Var m) | n == m    = a
                  | otherwise = Var m
subst a n (Cons c es) = Cons c (map (subst a n) es)
subst a n (App  x y ) = App (subst a n x) (subst a n y)
subst a n (Abs p e) | p `binds` n = Abs p e
                    | otherwise   = Abs p (subst a n e)

subst a n (Let p b e) | p `binds` n = Let p b e
                      | otherwise   = Let p (subst a n b) (subst a n e)

subst a n (LetRec alts e)
  | any ((`binds` n) . fst) alts = LetRec alts e
  | otherwise = LetRec (mapSnd (subst a n) alts) (subst a n e)

subst a n (Fatbar x y) = Fatbar (subst a n x) (subst a n y)
subst a n (If b t e  ) = If (subst a n b) (subst a n t) (subst a n e)
-- if the case is scrutinising this variable, rebind it with a let and
-- substitute inside the case.
subst a n (Case v alts)
  | n == v    = Let (VarPat v) a (Case v (map (substClause a n) alts))
  | otherwise = Case v (map (substClause a n) alts)
subst _a _n Fail                = Fail
subst _a _n (Bottom s         ) = Bottom s
subst a  n  (Project ar i e   ) = Project ar i (subst a n e)
subst a  n  (Y      e         ) = Y (subst a n e)
subst a  n  (Record fields    ) = Record (fmap (subst a n) fields)
subst a  n  (RecordProject e l) = RecordProject (subst a n e) l

-- If the clause rebinds the variable, don't substitute inside it
substClause :: Exp -> Name -> Clause -> Clause
substClause _ n (Clause c vars e) | n `elem` vars = Clause c vars e
substClause a n (Clause c vars e)                 = Clause c vars (subst a n e)

-- True if the variable is bound in the pattern
binds :: Pattern -> Name -> Bool
binds (ConstPat _   ) _ = False
binds (VarPat   m   ) n = m == n
binds (ConPat _ pats) n = any (`binds` n) pats

--------------------------------------------------------------------------------
-- The Pattern Match Compiler
-- Converts functions defined using pattern matching to nested case expressions.
--------------------------------------------------------------------------------

-- TODO: this code is not in the same style as the rest of the compiler and is
-- difficult to understand. Refactor it.

type Equation = ([Pattern], Exp)

-- The entrypoint to the pattern match compiler.
-- Converts expressions of the form
--    ((\p11...p1n -> e1) u1...un)
-- [] ((\pm1...pmn -> em) u1...un)
-- [] e
-- to a nested series of case expressions
match :: [Name] -> [Equation] -> Exp -> NameGen Exp
match []       qs def = pure $ foldr Fatbar def [ e | ([], e) <- qs ]
match (u : us) qs def = foldrM (matchVarCon (u : us)) def (groupOn isVar qs)

-- Given a constructor, return all constructors for that type
constructors :: Con -> [Con]
constructors Sum { sumFamily = f } = f
constructors prod                  = [prod]

rename :: Exp -> Name -> Name -> Exp
rename e j k = subst (Var j) k e

isVar :: Equation -> Bool
isVar (VarPat _ : _, _) = True
isVar _                 = False

getCon :: Equation -> Con
getCon (ConPat c _ : _, _) = c
getCon c                   = error $ "not a con: " <> show c

matchVarCon :: [Name] -> [Equation] -> Exp -> NameGen Exp
matchVarCon us qs def =
  let f = case head qs of
        (VarPat _            : _, _) -> matchVar
        (ConPat _ _          : _, _) -> matchCon
        (ConstPat (Int    _) : _, _) -> matchInt
        (ConstPat (String _) : _, _) -> matchString
        (ConstPat (Bool   _) : _, _) -> matchBool
        (ConstPat (Prim _) : _, _) ->
          error "ELC.Compile.matchVarCon: illegal primitive in pattern"
        ([], _) -> error "ELC.Compile.matchVarCon: unexpected empty list"
  in  f us qs def

matchVar :: [Name] -> [Equation] -> Exp -> NameGen Exp
matchVar (u : us) qs =
  match us [ (ps, rename e u v) | (VarPat v : ps, e) <- qs ]
matchVar [] _ = error "ELC.Compile.matchVar: unexpected empty list"

matchCon :: [Name] -> [Equation] -> Exp -> NameGen Exp
matchCon (u : us) qs def = do
  clauses <- mapM (\c -> matchClause c (u : us) (choose c qs) def) cs
  pure $ Case u clauses
  where cs = constructors (getCon (head qs))
matchCon [] _ _ = error "ELC.Compile.matchCon: unexpected empty list"

matchBool :: [Name] -> [Equation] -> Exp -> NameGen Exp
matchBool (u : us) qs def = do
  let (trueBranches, falseBranches) = foldl
        (\(ts, fs) q -> case q of
          (ConstPat (Bool True) : _, _) -> (ts <> [q], fs)
          (ConstPat (Bool False) : _, _) -> (ts, fs <> [q])
          _ -> error "ELC.Compile.matchBool: expected bool"
        )
        ([], [])
        qs
  trueClause  <- match us [ (ps, e) | (_ : ps, e) <- trueBranches ] def
  falseClause <- match us [ (ps, e) | (_ : ps, e) <- falseBranches ] def
  pure $ If (Var u) trueClause falseClause
matchBool [] _ _ = error "ELC.Compile.matchBool: unexpected empty list"

matchInt :: [Name] -> [Equation] -> Exp -> NameGen Exp
matchInt = error "ELC.Compile.matchInt: not implemented yet"

matchString :: [Name] -> [Equation] -> Exp -> NameGen Exp
matchString = error "ELC.Compile.matchString: not implemented yet"

matchClause :: Con -> [Name] -> [Equation] -> Exp -> NameGen Clause
matchClause con (_u : us) qs def = do
  us'  <- replicateM (conArity con) fresh
  expr <- match (us' ++ us)
                [ (ps' ++ ps, e) | (ConPat _ ps' : ps, e) <- qs ]
                def
  pure $ Clause con us' expr
matchClause _ [] _ _ = error "ELC.Compile.matchClause: unexpected empty list"

choose :: Con -> [Equation] -> [Equation]
choose c = filter ((== c) . getCon)
