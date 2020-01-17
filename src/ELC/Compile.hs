module ELC.Compile where

-- Compile Can.Exp to ELC.Exp

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Util
import           Control.Monad.State.Strict
import           Control.Monad                  ( replicateM )
import           Data.List                      ( sortBy )
import           Data.List.Extra                ( groupOn )
import           Data.Foldable                  ( foldlM
                                                , foldrM
                                                )

import           ELC
import           ELC.Primitive
import           Canonical                      ( Name(..) )
import qualified Canonical                     as Can
import qualified Syntax                        as S
import           Data.Name
import           NameGen                        ( NameGen
                                                , freshM
                                                , run
                                                )

-- defs are normal definitions
-- instances is a mapping from the name of the typeclass and the type(s) of the
-- instance to the name of the dictionary representing that instance.
--
-- TODO: whilst this looks reasonable on the surface, it turns out not to work
-- in all but the most trivial of cases. The main challenge here is that we need
-- to insert the correct dictionary into all typeclass method calls, based on
-- the type of the instance, which is inferred at typechecking time.
-- In the current architecture we have no access to inferred types in the
-- compile phase, so we have no idea which dictionary to insert.
--
-- I think the correct way to resolve this is for the typechecker to elaborate
-- the AST with type information, and for the compiler to operate on this
-- representation. This requires a significant restructuring as right now the
-- typechecker and the compiler use entirely orthogonal ASTs.
-- See https://www.youtube.com/watch?v=x3evzO8O9e8 for how GHC does this.
data Env = Env { defs :: Map Name Exp, instances :: Map (Name, [Can.Type]) Name }
  deriving (Show)

type InstMap = [((Name, [Can.Type]), Name)]

defaultEnv :: Env
defaultEnv = Env { defs      = Map.fromList primConstructors
                 , instances = Map.fromList primInstances
                 }

merge :: Env -> ([(Name, Exp)], InstMap) -> Env
merge env (newDefs, newInsts) = env
  { defs      = defs env <> Map.fromList newDefs
  , instances = instances env <> Map.fromList newInsts
  }

collapseEnv :: Env -> [(Name, Exp)]
collapseEnv = Map.toList . defs

lookupEnv :: Name -> Env -> Maybe Exp
lookupEnv n env = Map.lookup n (defs env)

lookupInstance :: Name -> [Can.Type] -> Env -> Maybe Name
lookupInstance n ts env = Map.lookup (n, ts) (instances env)

fresh :: NameGen Name
fresh = freshM (\i -> Local (Name ("$elc" ++ show i)))

freshTopLevel :: ModuleName -> NameGen Name
freshTopLevel m = freshM (\i -> TopLevel m (Name ("$elc" ++ show i)))

-- TODO: remove unreachable definitions
translateModule :: Env -> Can.Module Can.Exp -> NameGen Env
translateModule env S.Module { S.moduleDecls = decls } =
  -- To ensure the right things are in scope when needed, we process decls in a
  -- particular order:
  -- 1. data decls
  -- 2. typeclass decls
  -- 3. instance decls
  -- 4. everything else
  let ordering (S.DataDecl      _) _                   = LT
      ordering (S.TypeclassDecl _) (S.DataDecl _)      = GT
      ordering (S.TypeclassDecl _) _                   = LT
      ordering (S.TypeclassInst _) (S.DataDecl      _) = GT
      ordering (S.TypeclassInst _) (S.TypeclassDecl _) = GT
      ordering (S.TypeclassInst _) _                   = LT
      ordering (S.FunDecl       _) (S.Comment _)       = LT
      ordering (S.FunDecl       _) _                   = GT
      ordering _                   _                   = EQ
      orderedDecls = sortBy ordering decls
  in  foldlM (\e decl -> merge e <$> translateDecl e decl) env orderedDecls

translateDecl :: Env -> Can.Decl Can.Exp -> NameGen ([(Name, Exp)], InstMap)
translateDecl env (S.FunDecl S.Fun { S.funName = n, S.funDefs = defs }) = do
  let numVars = length (S.defArgs (head defs))
  varNames <- replicateM numVars fresh
  let vars = map VarPat varNames
  equations <- mapM (translateDef env) defs
  caseExpr  <- match varNames equations (Bottom "pattern match failed")
  pure ([(n, buildAbs caseExpr vars)], [])

translateDecl _env (S.DataDecl d) = do
  let cons = S.dataCons d
  datadefs <- if length cons > 1
    then
      let cs = zipWith (translateSumCon cs) [0 ..] cons
      in  pure $ map (\c -> (name c, Cons c [])) cs
    else concat <$> mapM translateProdCon cons
  pure (datadefs, [])
translateDecl env (S.TypeclassDecl t) = translateTypeclass env t
translateDecl env (S.TypeclassInst i) = translateInstance env i
translateDecl _   (S.Comment       _) = pure ([], [])

-- Translating typeclasses
-- -----------------------
-- Typeclasses get translated into record types. Each method corresponds to a
-- field in the record.
-- e.g.    class Monoid a where
--           empty : a
--           append : a -> a -> a
-- becomes data Monoid a = Monoid {
--           empty : a,
--           append : a -> a -> a }
translateTypeclass :: Env -> Can.Typeclass -> NameGen ([(Name, Exp)], InstMap)
translateTypeclass env t = do
  let record = S.Data
        { S.dataName   = S.typeclassName t
        , S.dataTyVars = map Can.fromLocal (S.typeclassTyVars t)
        , S.dataCons   = [ S.RecordCon { S.conName   = S.typeclassName t
                                       , S.conFields = S.typeclassDefs t
                                       }
                         ]
        }
  translateDecl env (S.DataDecl record)

-- Translating typeclass instances
-- -------------------------------
-- Typeclass instances get translated into record values.
-- e.g.    instance Monoid Int where
--           empty = 0
--           append x y = x + y
-- becomes $freshName : Monoid Int
--         $freshName = Monoid { empty = 0, append = \x y -> x + y }
--
-- The compiler will automatically generate selectors for each field, which
-- correspond to each typeclass method. The other half of this translation is
-- adding an extra argument to each use of a typeclass method, passing in the
-- correct instance record... and we can't do this unless we know the
-- instantiated type of each method call.
-- Note: we can't construct a friendly record name in general because not all
--       types can be easily converted to names. (e.g. a -> [(a, b)]).
-- Note: we register the instance in the env so it can be inserted into method
--       calls elsewhere in the program.
-- TODO: Currently the order of methods has to match the order in the typeclass
--       definition. Fix this.
--       (when we support { a = .. } style record construction we can use that)
translateInstance
  :: Env -> Can.Instance Can.Exp -> NameGen ([(Name, Exp)], InstMap)
translateInstance env i = do
  let typeclassName@(TopLevel moduleName rawTypeclassName) = S.instanceName i
  let recordType = fromMaybe
        (error $ "undefined typeclass " <> show typeclassName)
        (lookupEnv typeclassName env)
  let defs = S.instanceDefs i
  let defsAsFuns = map
        (\(name, defs) -> S.Fun { S.funComments   = []
                                , S.funName       = name
                                , S.funType       = S.TyHole "method"
                                , S.funConstraint = Nothing
                                , S.funDefs       = defs
                                }
        )
        defs
  defsAsExps <-
    concatMap fst <$> mapM (translateDecl env . S.FunDecl) defsAsFuns
  let record = foldl App recordType (map snd defsAsExps)
  recordName <- freshTopLevel moduleName
  pure
    ([(recordName, record)], [((typeclassName, S.instanceTypes i), recordName)])

-- Note: we do a weird trick here where each constructor has a reference to a
-- list of constructors that includes itself.
-- This is so that the pattern match compiler can perform exhaustiveness
-- checking on patterns that use the constructor.
translateSumCon :: [Con] -> Int -> Can.DataCon -> Con
translateSumCon f t S.DataCon { S.conName = n, S.conArgs = args } =
  Sum { name = n, tag = t, arity = length args, family = f }

translateProdCon :: Can.DataCon -> NameGen [(Name, Exp)]
translateProdCon S.DataCon { S.conName = n, S.conArgs = args } =
  pure [(n, Cons Prod { name = n, arity = length args } [])]
translateProdCon S.RecordCon { S.conName = n, S.conFields = fields } = do
  let constructor = Prod { name = n, arity = length fields }
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

-- Translate a function definition into a form understood by the pattern match
-- compiler.
translateDef :: Env -> Can.Def Can.Exp -> NameGen Equation
translateDef env def = do
  args <- mapM (translatePattern env) (S.defArgs def)
  expr <- translateExpr env (S.defExpr def)
  pure (args, expr)

translatePattern :: Env -> Can.Pattern -> NameGen Pattern
translatePattern _   (S.VarPat  n ) = pure (VarPat n)
translatePattern _   (S.IntPat  i ) = pure (ConstPat (Int i))
translatePattern env (S.ListPat es) = do
  pats <- mapM (translatePattern env) es
  pure (buildListPat pats)
translatePattern env (S.TuplePat es) = do
  pats <- mapM (translatePattern env) es
  pure (buildTuplePat pats)
translatePattern env (S.ConsPat n pats) = do
  pats' <- mapM (translatePattern env) pats
  pure $ ConPat (lookupCon n env) pats'
translatePattern _ S.WildPat = VarPat <$> fresh

lookupCon :: Name -> Env -> Con
lookupCon n env = case lookupEnv n env of
  Just (Cons c _) -> c
  _ -> error $ "unknown constructor: " <> show n <> "\n\n" <> pShow env

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

translateExpr :: Env -> Can.Exp -> NameGen Exp
translateExpr _   (S.IntLit i         ) = pure (Const (Int i) [])
translateExpr env (S.StringLit s parts) = translateStringLit env s parts
translateExpr env (S.ListLit elems    ) = do
  elems' <- mapM (translateExpr env) elems
  buildList elems'
translateExpr env (S.TupleLit elems) = do
  elems' <- mapM (translateExpr env) elems
  pure (buildTuple elems')

-- TODO: what's a better way to handle this?
-- possible have a Prelude module which puts these variables in scope, bound to
-- "$prim$Num$add" or something?
translateExpr _ (S.Var (TopLevel (ModuleName ["Lam", "Primitive"]) "+")) =
  binaryPrim PrimAdd
translateExpr _ (S.Var (TopLevel (ModuleName ["Lam", "Primitive"]) "*")) =
  binaryPrim PrimMult
translateExpr _ (S.Var (TopLevel (ModuleName ["Lam", "Primitive"]) "-")) =
  binaryPrim PrimSub
translateExpr _ (S.Var (TopLevel (ModuleName ["Lam", "Primitive"]) "appendString"))
  = binaryPrim PrimStringAppend

translateExpr _   (S.Var n  ) = pure (Var n)
translateExpr env (S.App a b) = do
  a' <- translateExpr env a
  b' <- translateExpr env b
  pure $ App a' b'
-- We translate a constructor into a series of nested lambda abstractions, one
-- for each argument to the constructor. When applied, the result is a fully
-- saturated constructor.
translateExpr env (S.Cons n) = do
  let con = lookupCon n env
      a   = arity con
  newVars <- replicateM a fresh
  pure $ buildAbs (Cons con (map Var newVars)) (map VarPat newVars)
translateExpr _   (S.Hole (Name n)) = pure $ Bottom ("Hole encountered: " <> n)
translateExpr env (S.Abs vars e   ) = do
  body <- translateExpr env e
  pure $ buildAbs body (map VarPat vars)
translateExpr env (S.Let alts expr) = do
  alts' <- mapM
    (\(n, e) -> do
      e' <- translateExpr env e
      pure (VarPat n, e')
    )
    alts
  expr' <- translateExpr env expr
  pure $ LetRec alts' expr'
-- case (foo bar) of
--   p1 -> e1
--   p2 -> e2
-- ==>
-- let $v = foo bar
--  in ((\p1 -> e1) $v) [] ((\p2 -> e2) $v) [] BOTTOM
-- ((\p1 -> e1) [] (\p2 -> e2)) (foo bar)
translateExpr env (S.Case scrut alts) = do
  var    <- fresh
  scrut' <- translateExpr env scrut
  alts'  <- mapM
    (\(p, e) -> do
      p' <- translatePattern env p
      e' <- translateExpr env e
      pure $ App (Abs p' e') (Var var)
    )
    alts
  let lams = foldr Fatbar (Bottom "pattern match failure") alts'
  pure $ Let (VarPat var) scrut' lams

-- "hi #{name}!" ==> "hi " <> show name <> "!"
translateStringLit :: Env -> String -> [(Can.Exp, String)] -> NameGen Exp
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
  go :: [(Can.Exp, String)] -> NameGen [Exp]
  go []            = pure []
  go ((e, s) : is) = do
    e'   <- translateExpr env e
    rest <- go is
    pure $ App (Var primShow) e' : Const (String s) [] : rest

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
-- if the case is scrutinising this variable, rebind it with a let and
-- substitute inside the case.
subst a n (Case v alts)
  | n == v    = Let (VarPat v) a (Case v (map (substClause a n) alts))
  | otherwise = Case v (map (substClause a n) alts)
subst _a _n Fail             = Fail
subst _a _n (Bottom s      ) = Bottom s
subst a  n  (Project ar i e) = Project ar i (subst a n e)
subst a  n  (Y e           ) = Y (subst a n e)

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
match (u : us) qs def = do
  foldrM (matchVarCon (u : us)) def (groupOn isVar qs)

-- Given a constructor, return all constructors for that type
constructors :: Con -> [Con]
constructors Sum { family = f } = f
constructors prod               = [prod]

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
  let f = if isVar (head qs) then matchVar else matchCon in f us qs def

matchVar :: [Name] -> [Equation] -> Exp -> NameGen Exp
matchVar (u : us) qs def =
  match us [ (ps, rename e u v) | (VarPat v : ps, e) <- qs ] def

matchCon :: [Name] -> [Equation] -> Exp -> NameGen Exp
matchCon (u : us) qs def = do
  clauses <- mapM (\c -> matchClause c (u : us) (choose c qs) def) cs
  pure $ Case u clauses
  where cs = constructors (getCon (head qs))

matchClause :: Con -> [Name] -> [Equation] -> Exp -> NameGen Clause
matchClause c (u : us) qs def = do
  us'  <- replicateM (arity c) fresh
  expr <- match (us' ++ us)
                [ (ps' ++ ps, e) | (ConPat c ps' : ps, e) <- qs ]
                def
  pure $ Clause c us' expr

choose :: Con -> [Equation] -> [Equation]
choose c qs = filter ((== c) . getCon) qs
