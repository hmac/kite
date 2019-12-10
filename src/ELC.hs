module ELC where

-- The Enriched Lambda Calculus
-- Taken from The Implementation of Functional Programming Languages, Chapter 3

import           Data.List.Extra                ( groupOn )
import           Data.Bifunctor                 ( second )
import           Data.List                      ( partition
                                                , mapAccumL
                                                )
import           Syntax                         ( Syn
                                                , Name(..)
                                                , Decl
                                                , Def
                                                , Module
                                                )
import qualified Syntax                        as S

data Exp = Const Constant
         | Var Name
         | Cons Con [Exp]
         | App Exp Exp
         | Abs Pattern Exp
         | Let Pattern Exp Exp
         | LetRec [(Pattern, Exp)] Exp
         | Fatbar Exp Exp
         | Case Name [Clause]
         | Fail
         | Bottom String
         | Project Int Int Exp   -- arity of the constructor; index of the field
         | Y Exp                 -- the Y combinator
         deriving (Show, Eq)

data Clause = Clause Con [Name] Exp deriving (Eq, Show)

data Pattern = ConstPat Constant
             | VarPat Name
             | ConPat Con [Pattern]
         deriving (Show, Eq)

data Con = Prod { name :: Name, arity :: Int }
         | Sum { name :: Name, tag :: Int, arity :: Int, family :: [Con] }

-- Because Sums contain infinite loops via family, we need to manually write Eq
-- and Show instances.
instance Eq Con where
  Prod { name = n, arity = a } == Prod { name = n', arity = a' } =
    n == n' && a == a'
  Sum { name = n, tag = t, arity = a } == Sum { name = n', tag = t', arity = a' }
    = n == n' && t == t' && a == a'
  _ == _ = False
instance Show Con where
  show Prod { name = n, arity = a } =
    "Prod { name = " <> show n <> ", arity = " <> show a <> " }"
  show Sum { name = n, tag = t, arity = a } =
    "Sum { name = "
      <> show n
      <> ", tag = "
      <> show t
      <> ", arity = "
      <> show a
      <> " }"

-- Primitive constructors: lists and tuples

listNil :: Con
listNil = Sum { name = "[]", tag = 0, arity = 0, family = [listNil, listCons] }
listCons :: Con
listCons =
  Sum { name = "::", tag = 1, arity = 2, family = [listNil, listCons] }
tuple2 :: Con
tuple2 = Prod { name = "(,)", arity = 2 }
tuple3 :: Con
tuple3 = Prod { name = "(,,)", arity = 3 }
tuple4 :: Con
tuple4 = Prod { name = "(,,,)", arity = 4 }
tuple5 :: Con
tuple5 = Prod { name = "(,,,,)", arity = 5 }
tuple6 :: Con
tuple6 = Prod { name = "(,,,,,)", arity = 6 }

primConstructors :: Env
primConstructors = map
  extract
  [listNil, listCons, tuple2, tuple3, tuple4, tuple5, tuple6]
  where extract con = (name con, Cons con [])

data Constant = Int Int
              | String String
              | Float Float
             | Prim Primitive
         deriving (Show, Eq)

data Primitive = PrimStringConcat
               | PrimShow
               | PrimStringAppend
         deriving (Show, Eq)

type Env = [(Name, Exp)]

-- TODO: convert all this to use a State monad over Int, like LC.
fresh :: Int -> Name
fresh k = Name $ "$elc" ++ show k

translateModule :: Int -> Env -> Module Syn -> Env
translateModule k env S.Module { S.moduleDecls = decls } =
  -- to ensure that all data types are in scope, we process data decls first
  let isDataDecl (S.DataDecl _) = True
      isDataDecl _              = False
      orderedDecls =
          let (dataDecls, otherDecls) = partition isDataDecl decls
          in  dataDecls ++ otherDecls
  in  foldl (\env' d -> env' ++ translateDecl k env' d) env orderedDecls

translateDecl :: Int -> Env -> Decl Syn -> [(Name, Exp)]
translateDecl k env (S.FunDecl S.Fun { S.funName = n, S.funDefs = defs }) =
  let numVars          = length (S.defArgs (head defs))
      varNames         = map (\i -> fresh (k + i)) [1 .. numVars]
      vars             = map VarPat varNames
      equations        = map (translateDef' (k + numVars) env) defs
      -- TODO: fix k usage here
      asCaseExpression = buildAbs
        (match (k + 100) varNames equations (Bottom "pattern match failed"))
        vars
  in  [(n, asCaseExpression)]

translateDecl _k _env (S.DataDecl d) =
  let cons = S.dataCons d
  in  if length cons > 1
        then
          let cs = zipWith (translateSumCon cs) [0 ..] cons
          in  map (\c -> (name c, Cons c [])) cs
        else map translateProdCon cons
translateDecl _ _ (S.TypeclassDecl _) = error "cannot translate typeclasses"
translateDecl _ _ (S.TypeclassInst _) =
  error "cannot translate typeclass instances"
translateDecl _ _ (S.Comment _) = []

-- Note: we do a weird trick here where each constructor has a reference to a
-- list of constructors that includes itself.
-- This is so that the pattern match compiler can perform exhaustiveness
-- checking on patterns that use the constructor.
translateSumCon :: [Con] -> Int -> S.DataCon -> Con
translateSumCon f t S.DataCon { S.conName = n, S.conArgs = args } =
  Sum { name = n, tag = t, arity = length args, family = f }

translateProdCon :: S.DataCon -> (Name, Exp)
translateProdCon S.DataCon { S.conName = n, S.conArgs = args } =
  (n, Cons Prod { name = n, arity = length args } [])

translateDef :: Int -> Env -> [Name] -> Def Syn -> Exp
translateDef k env vars def =
  let (k'  , args) = mapAccumL (translatePattern env) k (S.defArgs def)
      (_k'', expr) = translateExpr env k' (S.defExpr def)
  in  buildApp (buildAbs expr args) (map Var vars)

-- Translate a function definition into a form understood by the pattern match
-- compiler.
translateDef' :: Int -> Env -> Def Syn -> Equation
translateDef' k env def =
  let (k'  , args) = mapAccumL (translatePattern env) k (S.defArgs def)
      (_k'', expr) = translateExpr env k' (S.defExpr def)
  in  (args, expr)


buildAbs :: Exp -> [Pattern] -> Exp
buildAbs = foldr Abs

buildApp :: Exp -> [Exp] -> Exp
buildApp = foldl App

translatePattern :: Env -> Int -> S.Pattern -> (Int, Pattern)
translatePattern _ k (S.VarPat n) = (k, VarPat n)
translatePattern _ k (S.IntPat i) = (k, ConstPat (Int i))
translatePattern env k (S.ListPat es) =
  let (k', pats) = mapAccumL (translatePattern env) k es
  in  (k', buildListPat pats)
translatePattern env k (S.TuplePat es) =
  let (k', pats) = mapAccumL (translatePattern env) k es
  in  (k', buildTuplePat pats)
translatePattern env k (S.ConsPat n pats) =
  let (k', pats') = mapAccumL (translatePattern env) k pats
  in  (k', ConPat (lookupCon n env) pats')
translatePattern _ k S.WildPat = (k + 1, VarPat (fresh k))

lookupCon :: Name -> Env -> Con
lookupCon n env = case lookup n env of
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

translateExpr :: Env -> Int -> Syn -> (Int, Exp)
translateExpr _   k (S.IntLit   i       ) = (k, Const (Int i))
translateExpr _   k (S.FloatLit i       ) = (k, Const (Float i))
translateExpr env k (S.StringLit s parts) = translateStringLit env k s parts
translateExpr env k (S.ListLit elems) =
  second buildList (mapAccumL (translateExpr env) k elems)
translateExpr env k (S.TupleLit elems) =
  second buildTuple (mapAccumL (translateExpr env) k elems)
translateExpr _ k (S.Var n) = (k, Var n)
translateExpr env k (S.App a b) =
  let (k' , a') = translateExpr env k a
      (k'', b') = translateExpr env k' b
  in  (k'', App a' b')
-- TODO: check/rethink
translateExpr env k (S.Cons n       ) = (k, Cons (lookupCon n env) [])
translateExpr _   k (S.Hole (Name n)) = (k, Bottom $ "Hole encountered: " <> n)
translateExpr env k (S.Abs vars e) =
  let (k', body) = translateExpr env k e
  in  (k', buildAbs body (map VarPat vars))
translateExpr env k (S.Let alts expr) =
  -- TODO: refactor
  let (k', alts') = mapAccumL
        (\j (n, e) ->
          let (j', e') = translateExpr env j e in (j', (VarPat n, e'))
        )
        k
        alts
      (k'', expr') = translateExpr env k' expr
  in  (k'', LetRec alts' expr')
-- case (foo bar) of
--   p1 -> e1
--   p2 -> e2
-- ==>
-- let $v = foo bar
--  in ((\p1 -> e1) $v) [] ((\p2 -> e2) $v) [] BOTTOM
-- ((\p1 -> e1) [] (\p2 -> e2)) (foo bar)
translateExpr env k (S.Case scrut alts) =
  let var            = fresh k
      k'             = k + 1
      (k'' , scrut') = translateExpr env k' scrut
      (k''', alts' ) = mapAccumL
        (\j (p, e) ->
          let (j' , p') = translatePattern env j p
              (j'', e') = translateExpr env j' e
          in  (j'', App (Abs p' e') (Var var))
        )
        k''
        alts
      lams = foldl Fatbar (Bottom "pattern match failure") alts'
  in  (k''', Let (VarPat var) scrut' lams)
-- TODO: remove the below when we know the above is OK
-- case (foo bar) of
--   p1 -> e1
--   p2 -> e2
-- ==>
-- (\v1 -> case v1 of
--          p1 -> e1
--          p2 -> e2) (foo bar)
-- translateExpr env k (S.Case scrut alts) =
--   let var         = fresh k
--       (k', alts') = mapAccumL
--         (\j (p, e) ->
--           let (j' , p') = translatePattern env j p
--               (j'', e') = translateExpr env j' e
--           in  (j'', (p', e'))
--         )
--         (k + 1)
--         alts
--       lam           = Abs (VarPat var) (Case var alts')
--       (k'', scrut') = translateExpr env k' scrut
--   in  (k'', App lam scrut')

-- "hi #{name}!" ==> "hi " <> show name <> "!"
translateStringLit :: Env -> Int -> String -> [(S.Syn, String)] -> (Int, Exp)
translateStringLit env k prefix parts =
  let (k', rest) = go k parts
  in  ( k'
      , foldr1 (\x acc -> App (App (Const (Prim PrimStringAppend)) x) acc)
               (Const (String prefix) : rest)
      )
 where
  go j [] = (j, [])
  go j ((e, s) : is) =
    let (j' , e'  ) = translateExpr env j e
        (j'', rest) = go j' is
    in  (j'', App (Const (Prim PrimShow)) e' : Const (String s) : rest)

buildList :: [Exp] -> Exp
buildList =
  foldr (\e acc -> App (App (Cons listCons []) e) acc) (Cons listNil [])

buildTuple :: [Exp] -> Exp
buildTuple elems = case length elems of
  2 -> Cons tuple2 elems
  3 -> Cons tuple3 elems
  4 -> Cons tuple4 elems
  5 -> Cons tuple5 elems
  6 -> Cons tuple6 elems
  n -> error $ "cannot handle tuples of length " <> show n

eval :: Env -> Exp -> Exp
eval env (App (Abs (ConstPat k) e) a) | eval env a == Const k    = eval env e
                                      | (Bottom s) <- eval env a = Bottom s
                                      | otherwise                = Fail

eval env (App (Abs (ConPat c pats) e) (Cons c' args)) = if c == c'
  then let f = eval env (buildAbs e pats) in eval env (buildApp f args)
  else Fail
eval _env (App (Abs (ConPat _ _) _) (Bottom s)) = Bottom s
eval env (App (Abs (ConPat c pats) e) a) =
  eval env $ App (Abs (ConPat c pats) e) (eval env a)
eval env (App (Abs  (VarPat v) e    ) a) = eval env (subst a v e)
eval env (App (Cons c          args ) e) = Cons c (args ++ [eval env e])
eval env (App (Const (Prim PrimShow)) a) = primShow (eval env a)
eval env (App (App (Const (Prim PrimStringAppend)) a) b) =
  primStringAppend (eval env a) (eval env b)
eval env (App    a b) = eval env $ App (eval env a) b

eval env (Fatbar a b) = case eval env a of
  Fail       -> eval env b
  (Bottom s) -> Bottom s
  e          -> e
eval _env (Abs p e) = Abs p e
eval env  (Var v  ) = case lookup v env of
  Just e  -> eval env e
  Nothing -> Bottom $ "unknown variable: " ++ show v
eval _    (Cons c args    )           = Cons c args
eval _    (Const c        )           = Const c
-- let v1..vn = e in b ==> (\v1..vn -> b) e
eval env (Let v bind body) = let lam = Abs v body in eval env (App lam bind)
eval _env Fail                        = Fail
eval _env (Bottom s                 ) = Bottom s
eval env  (Case   n     alts        ) = evalCase env alts (eval env (Var n))
eval _env (LetRec _alts _body       ) = error "cannot evaluate letrecs"
eval env  (Project _ i (Cons _ args)) = eval env (args !! i)
eval env  (Project a i e            ) = eval env (Project a i (eval env e))
eval env  (Y e                      ) = eval env (App e (Y e))

evalCase :: Env -> [Clause] -> Exp -> Exp
evalCase _env [] _ = Bottom "case match failure"
evalCase env (Clause c vars e : alts) (Cons c' args)
  | c == c'   = let boundVars = zip vars args in eval (boundVars ++ env) e
  | otherwise = evalCase env alts (Cons c' args)
evalCase _ _ e = error $ "evalCase: expected constructor but found " <> show e

primShow :: Exp -> Exp
primShow Fail       = Fail
primShow (Bottom s) = Bottom s
primShow e          = Const $ String $ go e
 where
  go (Const (Int    i)) = show i
  go (Const (Float  f)) = show f
  go (Const (String s)) = s
  go (Const (Prim   _)) = "<builtin>"
  go (Abs  _ _        ) = "<function>"
  go (Cons c args) = let Name n = name c in n <> " " <> unwords (map go args)
  go _                  = "<unevaluated>"

primStringConcat :: Env -> Exp -> Exp
primStringConcat env expr = Const (String (go expr))
 where
  go (Const _) = ""
  go (App (App _cons (Const (String x))) xs) = x ++ go (eval env xs)
  go e = error $ "unexpected argument to primStringConcat: " <> show e

primStringAppend :: Exp -> Exp -> Exp
primStringAppend (Const (String a)) (Const (String b)) =
  Const (String (a <> b))
primStringAppend a b =
  error
    $  "unexpected arguments to primStringConcat: "
    <> show a
    <> "\n\n"
    <> show b

subst :: Exp -> Name -> Exp -> Exp
subst _ _ (Const c) = Const c
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
-- TODO: what do we do if the case is scrutinising this variable?
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

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f = map (\(x, y) -> (x, f y))

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
match :: Int -> [Name] -> [Equation] -> Exp -> Exp
match _k []       qs def = foldr Fatbar def [ e | ([], e) <- qs ]
match k  (u : us) qs def = foldr (matchVarCon k (u : us)) def (groupOn isVar qs)

-- Given a constructor, return all constructors for that type
constructors :: Con -> [Con]
constructors Sum { family = f } = f
constructors prod               = [prod]

rename :: Exp -> Name -> Name -> Exp
rename e j k = ELC.subst (Var j) k e

isVar :: Equation -> Bool
isVar (VarPat _ : _, _) = True
isVar _                 = False

getCon :: Equation -> Con
getCon (ConPat c _ : _, _) = c
getCon c                   = error $ "not a con: " <> show c

matchVarCon :: Int -> [Name] -> [Equation] -> Exp -> Exp
matchVarCon k us qs def =
  let f = if isVar (head qs) then matchVar else matchCon in f k us qs def

matchVar :: Int -> [Name] -> [Equation] -> Exp -> Exp
matchVar k (u : us) qs def =
  match k us [ (ps, rename e u v) | (VarPat v : ps, e) <- qs ] def

matchCon :: Int -> [Name] -> [Equation] -> Exp -> Exp
matchCon k (u : us) qs def = Case
  u
  [ matchClause c k (u : us) (choose c qs) def | c <- cs ]
  where cs = constructors (getCon (head qs))

matchClause :: Con -> Int -> [Name] -> [Equation] -> Exp -> Clause
matchClause c k (u : us) qs def =
  let exp = match (k' + k)
                  (us' ++ us)
                  [ (ps' ++ ps, e) | (ConPat c ps' : ps, e) <- qs ]
                  def
  in  Clause c us' exp
 where
  k'  = arity c
  us' = [ fresh (i + k) | i <- [1 .. k'] ]

choose :: Con -> [Equation] -> [Equation]
choose c qs = filter ((== c) . getCon) qs
