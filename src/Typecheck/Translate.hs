module Typecheck.Translate where

-- Convert types in Syntax to types in THIH

import qualified Data.List                     as List
                                                ( find )
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                )
import qualified Syntax                        as S
import           Typecheck.THIH
import           Typecheck.Desugar              ( Core )
import qualified Typecheck.Desugar             as D
import qualified Typecheck.Primitive           as Prim

-- TODO: this module is a mess with no clear entrypoint.
-- Restrict the exports and clean this up.

--          type cons     data cons
type Env = ([(Id, Type)], [(Id, Scheme)])

lookupTyCon :: Id -> Env -> Maybe Type
lookupTyCon n (tycons, _) = lookup n tycons

lookupDataCon :: Id -> Env -> Maybe Scheme
lookupDataCon n (_, datacons) = lookup n datacons

mergeEnv :: Env -> Env -> Env
mergeEnv (a, b) (c, d) = (a <> c, b <> d)

typeConstructors :: S.Module Core -> [(Id, Type)]
typeConstructors m = map constructorToType $ mapMaybe getData (S.moduleDecls m)
 where
  getData (S.DataDecl d) = Just d
  getData _              = Nothing
  constructorToType d = (name, TCon $ Tycon name conKind)
   where
    -- Note: this assumes that type variables all have kind *, so it doesn't
    -- support things like monad transformers (where m has kind * -> *)
    conKind = foldr (Kfun . const Star) Star (S.dataTyVars d)
    name    = toId (S.dataName d)

-- TODO: superclass constraints
addTypeclasses :: [S.Typeclass] -> EnvTransformer
addTypeclasses ts classEnv = foldl f (Right classEnv) ts
 where
  f mce t = case mce of
    Left  err -> Left err
    Right ce  -> addClass (S.unName (S.typeclassName t)) [] ce

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
toBindGroup env  (S.FunDecl       f) = Just (funToBindGroup env f)
toBindGroup _env (S.DataDecl      _) = Nothing
toBindGroup _env (S.Comment       _) = Nothing
toBindGroup _env (S.TypeclassDecl _) = Nothing
toBindGroup _env (S.TypeclassInst _) = Nothing

typeclassMethods :: Env -> S.Typeclass -> [Assump]
typeclassMethods env t = flip map (S.typeclassDefs t)
  $ \(name, ty) -> S.unName name :>: tyToScheme env ty (Just constraint)
 where
  -- In a typeclass definition
  -- class Eq a where
  --   eq : a -> a -> Bool
  -- we need to add a constraint to get
  -- eq : Eq a => a -> a -> Bool
  constraint = S.CInst (S.typeclassName t) (map S.TyVar (S.typeclassTyVars t))

funToBindGroup :: Env -> S.Fun Core -> BindGroup
funToBindGroup env f = case S.funType f of
  S.TyHole _ -> ([], [[funToImpl env f]])
  _          -> ([funToExpl env f], [])

funToImpl :: Env -> S.Fun Core -> Impl
funToImpl env f = (toId (S.funName f), map (defToAlt env) (S.funDefs f))

funToExpl :: Env -> S.Fun Core -> Expl
funToExpl env f =
  ( toId (S.funName f)
  , tyToScheme env (S.funType f) (S.funConstraint f)
  , map (defToAlt env) (S.funDefs f)
  )

tyToScheme :: Env -> S.Ty -> Maybe S.Constraint -> Scheme
tyToScheme env (S.TyList t) c = quantify
  (tv t')
  (constraintToPreds env c :=> list t')
  where t' = tyToType env t
tyToScheme env t c =
  quantify (tv (tyToType env t)) (constraintToPreds env c :=> tyToType env t)

-- currently we only support single parameter typeclasses
constraintToPreds :: Env -> Maybe S.Constraint -> [Pred]
constraintToPreds _ Nothing = []
constraintToPreds env (Just (S.CInst (S.Name className) [ty])) =
  [IsIn className (tyToType env ty)]
constraintToPreds _ (Just (S.CInst _ vs))
  | length vs > 1 = error
    "cannot handle multi parameter typeclass instances yet"
  | otherwise = error "cannot handle zero parameter typeclass instance"
constraintToPreds env (Just (S.CTuple a b)) =
  constraintToPreds env (Just a) <> constraintToPreds env (Just b)

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
toPat _env (S.VarPat n)     = PVar (toId n)
toPat _env S.WildPat        = PWildcard
toPat _env (S.IntPat  n   ) = PLit (LitInt (toInteger n))
toPat env  (S.ListPat pats) = foldr
  (\pat acc -> PCon Prim.listCons [toPat env pat, acc])
  (PCon Prim.listNil [])
  pats
toPat env (S.TuplePat pats) =
  let tupleCon = case length pats of
        2 -> Prim.tuple2
        3 -> Prim.tuple3
        4 -> Prim.tuple4
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
toExpr env  (D.App a     b ) = Ap (toExpr env a) (toExpr env b)
toExpr env  (D.Let binds e ) = Let (bindsToBindGroup env binds) (toExpr env e)
toExpr _env (D.IntLit    i ) = Lit (LitInt (toInteger i))
toExpr _env (D.FloatLit  f ) = Lit (LitFloat f)
toExpr _env (D.StringLit s ) = Lit (LitStr s)
toExpr env  (D.List      es) = foldr
  (\x acc -> Ap (Ap (Const Prim.listCons) (toExpr env x)) acc)
  (Const Prim.listNil)
  es
toExpr env (D.Tuple es) =
  let c = case length es of
        2 -> Prim.tuple2
        3 -> Prim.tuple3
        4 -> Prim.tuple4
        5 -> Prim.tuple5
        6 -> Prim.tuple6
        7 -> Prim.tuple7
        n -> error $ "cannot translate tuples of length " <> show n
  in  foldl Ap (Const c) (map (toExpr env) es)

bindsToBindGroup :: Env -> [(S.Name, [([S.Pattern], Core)])] -> BindGroup
bindsToBindGroup env binds = ([], map ((: []) . toBind) binds)
 where
  toBind :: (S.Name, [([S.Pattern], Core)]) -> Impl
  toBind (name, alts) = (toId name, map toAlt alts)
  toAlt :: ([S.Pattern], Core) -> Alt
  toAlt (pats, e) = (map (toPat env) pats, toExpr env e)

toId :: S.Name -> Id
toId (S.Name n) = n
