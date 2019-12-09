{-# LANGUAGE OverloadedStrings #-}
module ELC where

-- The Enriched Lambda Calculus
-- Taken from The Implementation of Functional Programming Languages, Chapter 3

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
         | Case Name [(Pattern, Exp)]
         | Fail
         | Bottom String
         deriving (Show, Eq)

data Pattern = ConstPat Constant
             | VarPat Name
             | ConPat Con [Pattern]
         deriving (Show, Eq)

data Con = Prod { name :: Name, arity :: Int }
         | Sum { name :: Name, tag :: Int, arity :: Int }
  deriving (Show, Eq)

-- Primitive constructors: lists and tuples

listNil :: Con
listNil = Sum { name = "[]", tag = 0, arity = 0 }
listCons :: Con
listCons = Sum { name = "::", tag = 1, arity = 2 }
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
  let numVars  = length (S.defArgs (head defs))
      varNames = map (\i -> fresh (k + i)) [1 .. numVars]
      vars     = map VarPat varNames
  in  [ ( n
        , buildAbs
          (foldl1
            Fatbar
            (  map (translateDef (k + numVars) env varNames) defs
            ++ [Bottom "pattern match failed"]
            )
          )
          vars
        )
      ]
translateDecl _k _env (S.DataDecl d) =
  let cons = S.dataCons d
  in  if length cons > 1
        then zipWith translateSumCon [0 ..] cons
        else map translateProdCon cons
translateDecl _ _ (S.TypeclassDecl _) = error "cannot translate typeclasses"
translateDecl _ _ (S.TypeclassInst _) =
  error "cannot translate typeclass instances"
translateDecl _ _ (S.Comment _) = []

translateSumCon :: Int -> S.DataCon -> (Name, Exp)
translateSumCon t S.DataCon { S.conName = n, S.conArgs = args } =
  (n, Cons Sum { name = n, tag = t, arity = length args } [])

translateProdCon :: S.DataCon -> (Name, Exp)
translateProdCon S.DataCon { S.conName = n, S.conArgs = args } =
  (n, Cons Prod { name = n, arity = length args } [])

translateDef :: Int -> Env -> [Name] -> Def Syn -> Exp
translateDef k env vars def =
  let (k'  , args) = mapAccumL (translatePattern env) k (S.defArgs def)
      (_k'', expr) = translateExpr env k' (S.defExpr def)
  in  buildApp (buildAbs expr args) (map Var vars)

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
-- (\v1 -> case v1 of
--          p1 -> e1
--          p2 -> e2) (foo bar)
translateExpr env k (S.Case scrut alts) =
  let var         = fresh k
      (k', alts') = mapAccumL
        (\j (p, e) ->
          let (j' , p') = translatePattern env j p
              (j'', e') = translateExpr env j' e
          in  (j'', (p', e'))
        )
        (k + 1)
        alts
      lam           = Abs (VarPat var) (Case var alts')
      (k'', scrut') = translateExpr env k' scrut
  in  (k'', App lam scrut')

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
eval _    (Cons c args    ) = Cons c args
eval _    (Const c        ) = Const c
-- let v1..vn = e in b ==> (\v1..vn -> b) e
eval env  (Let v bind body) = let lam = Abs v body in eval env (App lam bind)
eval _env Fail              = Fail
eval _env (Bottom s)        = Bottom s
-- case v of { p1 -> e1; p2 -> e2 } ==> ((\p1 -> e1) v) [] ((\p2 -> e2) v)
eval _env (Case n alts) =
  foldl (\acc (p, e) -> Fatbar acc (App (Abs p e) (Var n))) Fail alts
eval _env (LetRec _alts _body) = error "cannot evaluate letrecs"

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
subst _a _n (Case _v _alts) =
  error "cannot substitute into case expressions yet"
subst _a _n Fail       = Fail
subst _a _n (Bottom s) = Bottom s

-- True if the variable is bound in the pattern
binds :: Pattern -> Name -> Bool
binds (ConstPat _   ) _ = False
binds (VarPat   m   ) n = m == n
binds (ConPat _ pats) n = any (`binds` n) pats

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f = map (\(x, y) -> (x, f y))
