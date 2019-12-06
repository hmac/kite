{-# LANGUAGE OverloadedStrings #-}
module Translate where

-- Convert types in Syntax to types in THIH

import qualified Data.List                     as List
                                                ( find )
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                )
import qualified Syntax                        as S
import           THIH
import           Desugar                        ( Core )
import qualified Desugar                       as D

tiModule :: S.Module Core -> Either Error [Assump]
tiModule m = let (assumps, p) = toProgram m in tiProgram initialEnv assumps p

--          type cons     data cons
type Env = ([(Id, Type)], [(Id, Scheme)])

lookupTyCon :: Id -> Env -> Maybe Type
lookupTyCon n (tycons, _) = lookup n tycons

lookupDataCon :: Id -> Env -> Maybe Scheme
lookupDataCon n (_, datacons) = lookup n datacons

toProgram :: S.Module Core -> ([Assump], Program)
toProgram m =
  let tycons   = typeConstructors m
      datacons = primitiveConstructors ++ dataConstructors (tycons, []) m
      assumps  = map (uncurry (:>:)) datacons
      env      = (tycons, datacons)
  in  (assumps, mapMaybe (toBindGroup env) (S.moduleDecls m))

primitiveConstructors :: [(Id, Scheme)]
primitiveConstructors =
  let tuple (f :>: s) = (f, s)
  in  map tuple [listCons, primError, primStringConcat]

typeConstructors :: S.Module Core -> [(Id, Type)]
typeConstructors m = map constructorToType $ mapMaybe getData (S.moduleDecls m)
 where
  getData (S.DataDecl d) = Just d
  getData _              = Nothing
  constructorToType d = (name, TCon $ Tycon name conKind)
   where
    conKind = foldl Kfun Star (map (const Star) (S.dataTyVars d))
    name    = toId (S.dataName d)

-- These are the data constructors in the module
dataConstructors :: Env -> S.Module Core -> [(Id, Scheme)]
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

toBindGroup :: Env -> S.Decl Core -> Maybe BindGroup
toBindGroup env  (S.FunDecl       f) = Just ([funToExpl env f], [])
toBindGroup _env (S.DataDecl      _) = Nothing
toBindGroup _env (S.Comment       _) = Nothing
toBindGroup _env (S.TypeclassDecl _) = Nothing
toBindGroup _env (S.TypeclassInst _) = Nothing

funToExpl :: Env -> S.Fun Core -> Expl
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
tyToType _env (S.TyVar n)  = TVar (Tyvar (toId n) Star)
tyToType _env S.TyInt      = tInt
tyToType _env S.TyFloat    = tFloat
tyToType _env S.TyString   = tString
tyToType env  (S.TyList t) = list (tyToType env t)
tyToType env  (a S.:@: b ) = TAp (tyToType env a) (tyToType env b)
tyToType _env S.TyArr      = tArrow
tyToType env  (S.TyCon n)  = fromMaybe
  (error ("unknown type constructor " <> toId n))
  (lookupTyCon (toId n) env)
tyToType _env (S.TyHole _) = error "cannot translate holes yet"
tyToType env (S.TyTuple ts) =
  let ty = case length ts of
        2 -> tTuple2
        3 -> tTuple3
        4 -> tTuple4
        5 -> tTuple5
        6 -> tTuple6
        7 -> tTuple7
        n -> error $ "cannot translate tuple of length " ++ show n
  in  foldl TAp ty (map (tyToType env) ts)

lookupTycon :: S.Name -> [Assump] -> Type
lookupTycon name as = case List.find (\(n :>: _) -> n == toId name) as of
  Just (_ :>: Forall _ks (_preds :=> t)) -> t
  Nothing -> error $ "unbound tycon: " <> toId name <> "\n" <> show as

defToAlt :: Env -> S.Def Core -> Alt
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
toExpr :: Env -> Core -> Expr
toExpr _env (D.Var  n         ) = Var (toId n)
toExpr env  (D.Cons (S.Name c)) = case lookupDataCon c env of
  Just a  -> Const (c :>: a)
  Nothing -> error $ "unknown data constructor: " <> c
toExpr _env (D.Hole _      ) = error "cannot translate holes yet"
toExpr _env (D.Abs _v    _e) = error "cannot typecheck lambda abstractions yet"
toExpr env  (D.App a     b ) = Ap (toExpr env a) (toExpr env b)
toExpr env  (D.Let binds e ) = Let (bindsToBindGroup env binds) (toExpr env e)
toExpr _env (D.IntLit    i ) = Lit (LitInt (toInteger i))
toExpr _env (D.StringLit s ) = Lit (LitStr s)
toExpr env  (D.List      es) = foldr
  (\x acc -> Ap (Ap (Const listCons) (toExpr env x)) acc)
  (Const listNil)
  es
toExpr env (D.Tuple es) =
  let c = case length es of
        2 -> tuple2
        3 -> tuple3
        4 -> tuple4
        5 -> tuple5
        6 -> tuple6
        7 -> tuple7
        n -> error $ "cannot translate tuples of length " <> show n
  in  foldl Ap (Const c) (map (toExpr env) es)
toExpr env (D.List es) = foldr
  (\e acc -> Ap (Ap (Const listCons) (toExpr env e)) acc)
  (Const listNil)
  es
toExpr env (D.Case scrut alts) = error "cannot translate case expressions yet"

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
tuple5 :: Assump
tuple5 = "$prim_tuple5" :>: Forall
  [Star, Star, Star, Star, Star]
  (   []
  :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` foldl
        TAp
        tTuple5
        (map TGen [0 .. 4])
      )
  )
tuple6 :: Assump
tuple6 = "$prim_tuple6" :>: Forall
  [Star, Star, Star, Star, Star, Star]
  (   []
  :=> (    TGen 0
      `fn` TGen 1
      `fn` TGen 2
      `fn` TGen 3
      `fn` TGen 4
      `fn` TGen 5
      `fn` foldl TAp tTuple6 (map TGen [0 .. 5])
      )
  )
tuple7 :: Assump
tuple7 = "$prim_tuple7" :>: Forall
  [Star, Star, Star, Star, Star, Star, Star]
  (   []
  :=> (    TGen 0
      `fn` TGen 1
      `fn` TGen 2
      `fn` TGen 3
      `fn` TGen 4
      `fn` TGen 5
      `fn` TGen 6
      `fn` foldl TAp tTuple7 (map TGen [0 .. 6])
      )
  )

-- primitive list constructors
listNil :: Assump
listNil = "[]" :>: Forall [Star] ([] :=> TAp tList (TGen 0))
listCons :: Assump
listCons = "Cons" :>: Forall
  [Star]
  ([] :=> (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))

-- other primitive functions
primError :: Assump
primError = "error" :>: Forall [Star] ([] :=> (tString `fn` TGen 0))
primStringConcat :: Assump
primStringConcat =
  "$prim_stringconcat" :>: Forall [] ([] :=> (list tString `fn` tString))

bindsToBindGroup :: Env -> [(S.Name, Core)] -> BindGroup
bindsToBindGroup env binds = ([], map ((: []) . toBind) binds)
 where
  toBind :: (S.Name, Core) -> Impl
  toBind (name, expr) = (toId name, [([], toExpr env expr)])

toId :: S.Name -> Id
toId (S.Name n) = n
