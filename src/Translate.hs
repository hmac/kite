{-# LANGUAGE OverloadedStrings #-}
module Translate where

-- Convert types in Syntax to types in THIH

import qualified Data.List                     as List
                                                ( find )
import           Data.Maybe                     ( mapMaybe )
import qualified Syntax                        as S
import           THIH

tiModule :: S.Module -> Either Error [Assump]
tiModule m = let (assumps, p) = toProgram m in tiProgram initialEnv assumps p

--          type cons     data cons
type Env = ([(Id, Type)], [(Id, Scheme)])

lookupTyCon :: Id -> Env -> Maybe Type
lookupTyCon n (tycons, _) = lookup n tycons

lookupDataCon :: Id -> Env -> Maybe Scheme
lookupDataCon n (_, datacons) = lookup n datacons

toProgram :: S.Module -> ([Assump], Program)
toProgram m =
  let tycons   = typeConstructors m
      datacons = primitiveConstructors ++ dataConstructors (tycons, []) m
      assumps  = map (uncurry (:>:)) datacons
      env      = (tycons, datacons)
  in  (assumps, mapMaybe (toBindGroup env) (S.moduleDecls m))

primitiveConstructors :: [(Id, Scheme)]
primitiveConstructors = [let (cons :>: sch) = listCons in (cons, sch)]

typeConstructors :: S.Module -> [(Id, Type)]
typeConstructors m = map constructorToType $ mapMaybe getData (S.moduleDecls m)
 where
  getData (S.DataDecl d) = Just d
  getData _              = Nothing
  constructorToType d = (name, TCon $ Tycon name conKind)
   where
    conKind = foldl Kfun Star (map (const Star) (S.dataTyVars d))
    name    = toId (S.dataName d)

-- These are the data constructors in the module
dataConstructors :: Env -> S.Module -> [(Id, Scheme)]
dataConstructors env m = concatMap toAssumps
  $ mapMaybe getData (S.moduleDecls m)
 where
  getData (S.DataDecl d) = Just d
  getData _              = Nothing
  toAssumps :: S.Data -> [(Id, Scheme)]
  toAssumps d = map
    (toAssump (toId (S.dataName d))
              (map ((`Tyvar` Star) . toId) (S.dataTyVars d))
    )
    (S.dataCons d)
  -- data Maybe a = Nothing | Just a
  -- becomes:
  -- Nothing : forall a. Maybe a
  -- Just : forall a. a -> Maybe a
  toAssump :: Id -> [Tyvar] -> S.DataCon -> (Id, Scheme)
  toAssump tyName tyVars con = (toId (S.conName con), scheme)
   where
    scheme   = Forall (map kind tyVars) (apply subst ty)
    ty       = [] :=> foldr fn returnTy (map (tyToType env) (S.conArgs con))
    returnTy = foldl TAp tycon (map TVar tyVars)
    subst    = zip tyVars (map TGen [0 ..])
    tycon    = case lookupTyCon tyName env of
      Just t  -> t
      Nothing -> error $ "unknown type constructor " <> tyName

toBindGroup :: Env -> S.Decl -> Maybe BindGroup
toBindGroup env  (S.FunDecl  f) = Just ([funToExpl env f], [])
toBindGroup _env (S.DataDecl _) = Nothing
toBindGroup _env (S.Comment  _) = Nothing

funToExpl :: Env -> S.Fun -> Expl
funToExpl env f =
  ( toId (S.funName f)
  , tyToScheme env (S.funType f)
  , map (defToAlt env) (S.funDefs f)
  )

tyToScheme :: Env -> S.Ty -> Scheme
tyToScheme env (S.TyList t) = quantify (tv t') ([] :=> list t')
  where t' = tyToType env t
tyToScheme env t = quantify (tv (tyToType env t)) ([] :=> tyToType env t)

tyToType :: Env -> S.Ty -> Type
tyToType _env (S.TyVar n)    = TVar (Tyvar (toId n) Star)
tyToType _env S.TyInt        = tInt
tyToType _env S.TyFloat      = tFloat
tyToType _env S.TyString     = tString
tyToType env  (S.TyList t  ) = list (tyToType env t)
tyToType env (S.TyArr a b) = TAp (TAp tArrow (tyToType env a)) (tyToType env b)
tyToType env  (S.TyApp n ts) = case lookupTyCon (toId n) env of
  Just t  -> foldl TAp t (map (tyToType env) ts)
  Nothing -> error $ "unknown type constructor " <> toId n
tyToType _env (S.TyHole _) = error "cannot translate holes yet"
tyToType env (S.TyTuple ts) =
  let ty = case length ts of
        2 -> tTuple2
        3 -> tTuple3
        4 -> tTuple4
        n -> error $ "cannot translate tuple of length " ++ show n
  in  foldl TAp ty (map (tyToType env) ts)

lookupTycon :: S.Name -> [Assump] -> Type
lookupTycon name as = case List.find (\(n :>: _) -> n == toId name) as of
  Just (_ :>: Forall _ks (_preds :=> t)) -> t
  Nothing -> error $ "unbound tycon: " <> toId name <> "\n" <> show as

defToAlt :: Env -> S.Def -> Alt
defToAlt env d = (map (toPat env) (S.defArgs d), toExpr env (S.defExpr d))

-- TODO: pass in a context of constructors in scope, with their declared types
toPat :: Env -> S.Pattern -> Pat
toPat _env (S.VarPat n) = PVar (toId n)
toPat _env S.WildPat    = PWildcard
toPat _env (S.IntPat n) = PLit (LitInt (toInteger n))
toPat env (S.ListPat pats) =
  foldr (\pat acc -> PCon listCons [toPat env pat, acc]) (PCon listNil []) pats
toPat env (S.TuplePat pats) =
  let tupleCon = case length pats of
        2 -> tuple2
        3 -> tuple3
        4 -> tuple4
        n -> error $ "cannot translate tuple patterns of length " <> show n
  in  PCon tupleCon (map (toPat env) pats)
toPat env (S.ConsPat name pats) = PCon (lookupCon (toId name))
                                       (map (toPat env) pats)
 where
  lookupCon n = maybe (error ("unknown constructor " <> show n))
                      (n :>:)
                      (lookupDataCon n env)

-- At this point, Cons should hold the type information for each constructor so
-- we can pass it on to Assump
toExpr :: Env -> S.Syn -> Expr
toExpr _env (S.Var  n         ) = Var (toId n)
toExpr env  (S.Cons (S.Name c)) = case lookupDataCon c env of
  Just a  -> Const (c :>: a)
  Nothing -> error $ "unknown data constructor: " <> c
toExpr _env (S.Hole _   ) = error "cannot translate holes yet"
toExpr env  (S.Abs [v] e) = absToLet env [v] e
toExpr _env (S.Abs _vs _e) =
  error "cannot translate multi-variable lambdas yet"
toExpr env  (S.App a     b) = Ap (toExpr env a) (toExpr env b)
toExpr env  (S.Let binds e) = Let (bindsToBindGroup env binds) (toExpr env e)
toExpr _env (S.IntLit i   ) = Lit (LitInt (toInteger i))
toExpr env (S.TupleLit es) =
  let const = case length es of
        2 -> tuple2
        3 -> tuple3
        4 -> tuple4
        n -> error $ "cannot translate tuples of length " <> show n
  in  foldl Ap (Const const) (map (toExpr env) es)

-- Primitive tuple constructor types
tuple2 :: Assump
tuple2 = "$prim_tuple2" :>: Forall
  [Star, Star]
  ([] :=> (TGen 0 `fn` TGen 1 `fn` foldl TAp tTuple2 (map TGen [0 .. 1])))
tuple3 :: Assump
tuple3 = "$prim_tuple3" :>: Forall
  [Star, Star, Star]
  (   []
  :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` foldl TAp
                                                 tTuple3
                                                 (map TGen [0 .. 2])
      )
  )
tuple4 :: Assump
tuple4 = "$prim_tuple4" :>: Forall
  [Star, Star, Star, Star]
  (   []
  :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` foldl
        TAp
        tTuple4
        (map TGen [0 .. 3])
      )
  )

-- primitive list constructors
listNil :: Assump
listNil = "[]" :>: Forall [Star] ([] :=> TAp tList (TGen 0))
listCons :: Assump
listCons = "Cons" :>: Forall
  [Star]
  ([] :=> (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))


-- Convert (\x -> e)
-- to      (let f x = e in f)
-- TODO: do we need to worry about f clashing with an existing variable?
-- probably.
absToLet :: Env -> [S.Name] -> S.Syn -> Expr
absToLet env [v] e =
  Let ([], [[("$f", [([PVar (toId v)], toExpr env e)])]]) (Var "$f")
absToLet _ _ _ = error "cannot translate multi-variable lambdas yet"

bindsToBindGroup :: Env -> [(S.Name, S.Syn)] -> BindGroup
bindsToBindGroup env binds = ([], map ((: []) . toBind) binds)
 where
  toBind :: (S.Name, S.Syn) -> Impl
  toBind (name, expr) = (toId name, [([], toExpr env expr)])

toId :: S.Name -> Id
toId (S.Name n) = n
