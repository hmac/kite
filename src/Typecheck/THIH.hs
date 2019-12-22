{-# LANGUAGE MultiParamTypeClasses #-}
module Typecheck.THIH where

-- This module is taken from Typing Haskell in Haskell by Mark P Jones (Haskell
-- Workshop Vol 7, 1999).
-- https://archive.alvb.in/msc/03_infoafp/papers/2012-12-13_HoorCollege_TypingHaskellInHaskell_dk.pdf

-- It's got a lot of edge cases specific to Haskell that I'm hoping to smooth
-- off in time.

import           Control.Monad.Except           ( MonadError(..) )
import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( zipWithM )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , isNothing
                                                )
import           Data.List                      ( nub
                                                , (\\)
                                                , intersect
                                                , union
                                                , partition
                                                )
import           Data.Foldable                  ( asum )
import           Data.Name
import           Canonical                      ( Name(..) )
import           Data.String                    ( fromString )


-- The type of identifiers
type Id = Name

enumId :: Int -> Id
enumId n = Local $ fromString ("v" <> show n)

---------
-- Kinds
-- ------

-- Int :   *
-- Maybe : * -> *
data Kind = Star | Kfun Kind Kind
          deriving (Eq, Show)

---------
-- Types
---------

data Type = TVar Tyvar     -- type variables, e.g. a
          | TCon Tycon     -- type constructors, e.g. Maybe
          | TAp Type Type  -- type application, e.g. Maybe @ a
          | TGen Int       -- "generic", quantified type variables. TODO: example
          deriving (Eq, Show)

data Tyvar = Tyvar Id Kind
          deriving (Eq, Show)

data Tycon = Tycon Id Kind
          deriving (Eq, Show)

---------------------
-- Examples of types
---------------------

-- TODO: move these to Typecheck.Primitive or similar

tUnit, tChar, tString, tInt, tInteger, tFloat, tDouble, tBool :: Type
tUnit = TCon (Tycon (TopLevel modPrim "()") Star)
tChar = TCon (Tycon (TopLevel modPrim "Char") Star)
tString = TCon (Tycon (TopLevel modPrim "String") Star)
tInt = TCon (Tycon (TopLevel modPrim "Int") Star)
tInteger = TCon (Tycon (TopLevel modPrim "Integer") Star)
tFloat = TCon (Tycon (TopLevel modPrim "Float") Star)
tDouble = TCon (Tycon (TopLevel modPrim "Double") Star)
tBool = TCon (Tycon (TopLevel modPrim "Bool") Star)

tList, tArrow, tTuple2, tTuple3, tTuple4, tTuple5, tTuple6, tTuple7 :: Type
tList = TCon (Tycon (TopLevel modPrim "[]") (Kfun Star Star))
tArrow = TCon (Tycon (TopLevel modPrim "(->)") (Kfun Star (Kfun Star Star)))
tTuple2 = TCon (Tycon (TopLevel modPrim "(,)") (Kfun Star (Kfun Star Star)))
tTuple3 =
  TCon (Tycon (TopLevel modPrim "(,)") (Kfun Star (Kfun Star (Kfun Star Star))))
tTuple4 = TCon
  (Tycon (TopLevel modPrim "(,)")
         (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))
  )
tTuple5 = TCon
  (Tycon (TopLevel modPrim "(,)")
         (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))
  )
tTuple6 = TCon
  (Tycon
    (TopLevel modPrim "(,)")
    (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))
  )
tTuple7 = TCon
  (Tycon
    (TopLevel modPrim "(,)")
    (Kfun
      Star
      (Kfun Star
            (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))
      )
    )
  )

-------------------------------------------
-- Helper functions for constructing types
-------------------------------------------

infixr 4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

list :: Type -> Type
list t = TAp tList t

pair :: Type -> Type -> Type
pair a b = TAp (TAp tTuple2 a) b

----------------------------------
-- Determining the kind of a term
----------------------------------

class HasKind t where
  kind :: t -> Kind

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k

instance HasKind Type where
  kind (  TCon tc) = kind tc
  kind (  TVar u ) = kind u
  kind u@(TAp t _) = case kind t of
    (Kfun _ k) -> k
    Star ->
      error
        $  "invalid kind * on left of type application: "
        <> show t
        <> " in type "
        <> show u
  kind (TGen _) = error "kind: no case for TGen for some reason"

-----------------
-- Substitutions:
--   mappings from type variables to types
-----------------

type Subst = [(Tyvar, Type)]

-- The empty substitution
nullSubst :: Subst
nullSubst = mempty

-- A single variable substitution
-- Precondition: kind u == kind t
(+->) :: Tyvar -> Type -> Subst
u +-> t = [(u, t)]

----------------------------------------------------------
-- A class for applying substitutions to Type-like things
----------------------------------------------------------

class Types t where
  -- Apply a substitution. Replaces every occurrence of a type variable in the
  -- domain of the substitution with the corresponding type.
  apply :: Subst -> t -> t
  -- Calculate the set of type variables appearing in t, in order of first
  -- occurrence, with no duplicates.
  tv :: t -> [Tyvar]

instance Types Type where
  apply s (TVar v ) = fromMaybe (TVar v) (lookup v s)
  apply s (TAp l r) = TAp (apply s l) (apply s r)
  apply _ t         = t

  tv (TVar v ) = [v]
  tv (TAp l r) = tv l `union` tv r
  tv _         = []

instance Types a => Types [a] where
  apply s = map (apply s)
  tv = nub . concatMap tv

-- Composition of two substitutions
infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [ (u, apply s1 t) | (u, t) <- s2 ] <> s1

-- Merge two substitutions
-- The two substitutions must agree on the mapping of every variable in the
-- domain of both.
-- Postcondition: apply (s1 <> s2) == apply (s2 <> s1)
merge :: Subst -> Subst -> TI Subst
merge s1 s2 = if agree
  then pure (s1 <> s2)
  else throw (SubstitutionsDontAgree s1 s2)
 where
  agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
              (map fst s1 `intersect` map fst s2)

---------------------------------------------------
-- Unification
--  Finding a substitution to make two types equal
---------------------------------------------------

-- A substitution s is a unifier of two types t1 and t2 if apply s t1 == apply s t2.
-- A most general unifier of two types is a unifier u such that any other
-- unifier s can be written as s' @@ u for some substitution s'.

-- Calculate the most general unifier of two types
mgu :: Type -> Type -> TI Subst
mgu (TAp l r) (TAp l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  pure (s2 @@ s1)
mgu (TVar u) t                         = varBind u t
mgu t        (TVar u)                  = varBind u t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = pure nullSubst
mgu t1 t2                              = throw (CannotUnify t1 t2)

-- Unify a variable u with a type t.
-- We must check that t doesn't not mention u
-- and that t and u have the same kinds.
varBind :: Tyvar -> Type -> TI Subst
varBind u t | t == TVar u      = pure nullSubst
            | u `elem` tv t    = throw $ OccursCheckFailed u t
            | kind u /= kind t = throw $ KindsDontMatch u t
            | otherwise        = pure (u +-> t)

-- Matching: find a substitution s such that apply s t1 == t2.
-- Similar to unification but is one-way.
-- Follows the same pattern as unification but uses merge instead of @@ to
-- combine substitutions, and does not allow binding of variables in t2.
match :: Type -> Type -> TI Subst
match (TAp l r) (TAp l' r') = do
  sl <- match l l'
  sr <- match r r'
  merge sl sr
match (TVar u) t | kind u == kind t      = pure (u +-> t)
match (TCon tc1) (TCon tc2) | tc1 == tc2 = pure nullSubst
match t1 t2                              = throw $ TypesDontMatch t1 t2

----------------
-- Type Classes
----------------

-- A qualification of a type by a list of predicates (typeclass constraints)
-- In a value of the form ps :=> t, ps is the context and t is the head.
data Qual t = [Pred] :=> t
  deriving (Eq, Show)

-- Eq a ===> IsIn "Eq" (TVar (Tyvar "a" Star))
data Pred = IsIn Id Type
  deriving (Eq, Show)

instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t) = tv ps `union` tv t

instance Types Pred where
  apply s (IsIn i t) = IsIn i (apply s t)
  tv (IsIn _ t) = tv t

mguPred :: Pred -> Pred -> Maybe Subst
mguPred = lift mgu

matchPred :: Pred -> Pred -> Maybe Subst
matchPred = lift match

lift :: (Type -> Type -> TI a) -> Pred -> Pred -> Maybe a
lift m (IsIn i t) (IsIn i' t')
  | i == i' = case runTI (m t t') of
    Left  _ -> Nothing
    Right r -> Just r
  | otherwise = Nothing

-- Classes and Instances

type Class = ([Id], [Inst])
type Inst = Qual Pred

-- An example typeclass
exampleOrd :: Class
exampleOrd =
  ( [eqClass]
  , [ [] :=> IsIn (ordClass) tUnit
    , [] :=> IsIn (ordClass) tChar
    , [] :=> IsIn (ordClass) tInt
    , [ IsIn (ordClass) (TVar (Tyvar (Local "a") Star))
      , IsIn (ordClass) (TVar (Tyvar (Local "b") Star))
      ]
      :=> IsIn
            (ordClass)
            (pair (TVar (Tyvar (Local "a") Star))
                  (TVar (Tyvar (Local "b") Star))
            )
    ]
  )

----------------------
-- Class Environments
----------------------

data ClassEnv = ClassEnv
  { classes :: Id -> Maybe Class -- maps identifiers to Class values
  , defaults :: [Type] }         -- provides a list of types for defaulting

-- Partial functions for extracting class information from the environment
super :: ClassEnv -> Id -> [Id]
super ce i = case classes ce i of
  Just (is, _) -> is

insts :: ClassEnv -> Id -> [Inst]
insts ce i = case classes ce i of
  Just (_, its) -> its
  Nothing       -> error $ "unknown class " <> show i

-- Update a class environment to reflect a new binding of a Class value to a
-- given identifier.
modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce { classes = \j -> if i == j then Just c else classes ce j }

initialEnv :: ClassEnv
initialEnv =
  ClassEnv { classes = const Nothing, defaults = [tInteger, tDouble] }

-- As we process each class or instance declaration in a program we transform
-- the initial class environment to add entries. There's a possibility that the
-- new declaration might be incompatible with previous declarations, for example
-- if it attempts to redefine an existing class. Therefore we describe
-- transformations using the following type, with Maybe for modelling the
-- possibility of failure.
type EnvTransformer = ClassEnv -> Either Error ClassEnv

-- Note: (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

-- To add a new class to an environment we must check:
-- - there isn't already a class with that name
-- - all the named superclasses are already defined
addClass :: Id -> [Id] -> EnvTransformer
addClass i is ce
  | isJust (classes ce i)           = error "class already defined"
  | any (isNothing . classes ce) is = error "superclass not defined"
  | otherwise                       = pure (modify ce i (is, []))

-- To add a new instance to a class, we must check:
-- - the class is defined
-- - the instance doesn't overlap with any previously defined instance
addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce
  | isNothing (classes ce i) = throw (NoClassForInstance p)
  | any (overlap p) qs       = throw (OverlappingInstance p)
  | otherwise                = pure (modify ce i c)
 where
  its = insts ce i
  qs  = [ q | (_ :=> q) <- its ]
  c   = (super ce i, (ps :=> p) : its)

-- Two instances overlap if there is some predicate that is a substitution
-- instance of the heads of both instance declarations.
-- Example overlaps:
-- Eq Int       and Eq Int
-- Eq [Int]     and Eq [a]
-- Eq (a, Bool) and Eq (Int, b)
overlap :: Pred -> Pred -> Bool
overlap p q = isJust (mguPred p q)

-- The Haskell report imposes furthe restrictions on class and instance
-- declarations that are not checked here:
-- - The superclasses of a class should have the same kind as the class itself
-- - The parameters of any predicates in an instance context should be type
--   variables, each of which should appear in the head of the instance.
-- - The type appearing in the head of an instance should consist of a type
--   constructor applied to a sequence of distinct type variables.
--
-- These conditions have no direct impact on type checking and are
-- straightforward (if tedious) to verify. They should be checked during static
-- analysis before typechecking.

-------------------------------------------------------------------
-- Entailment
--  Answering questions about which types are instances of a class
-------------------------------------------------------------------

-- Given a predicate p and a list of predicates ps, we want to determine whether
-- p will hold whenever all of ps hold.

-- If a type is an instance of a class i, it must also be an instance of any
-- superclasses of i.
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t) =
  p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

-- This does something more complex involving checking the heads of relevant
-- instance declarations. See THIH p. 16 for a description.
byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i _) = asum [ tryInst it | it <- insts ce i ]
 where
  tryInst (ps :=> h) = do
    u <- matchPred h p
    Just (map (apply u) ps)

-- Now we can define the general entailment function.
-- Given a class environment ce, entail ce ps p == True iff p will hold whenever
-- all the predicates in ps hold.
-- This function will terminate because of restrictions in how classes and
-- instances can be defined:
-- - The class hierarchy must be acyclic
-- - The types in any instance declaration must be strictly smaller than those
--   in the head.
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) || case byInst ce p of
  Nothing -> False
  Just qs -> all (entail ce ps) qs


--------------------------------
-- Context Reduction
--   Simplifying predicate lists
--------------------------------

-- Examples:
-- Eq [a]        => Eq a
-- Eq (a, a)     => Eq a
-- Eq ([a], Int) => Eq a
--
-- This reduction is valid because, for any choice of a, each predicate holds
-- iff Eq a holds.

-- Haskell specifies that we simplify predicates until we obtain types in a sort
-- of "head-normal form":
-- Class arguments must be of the form
--   v t1 ... tn
-- where v is a type variable and t1,...,tn are types
inHnf :: Pred -> Bool
inHnf (IsIn _ t) = hnf t
 where
  hnf (TVar _ ) = True
  hnf (TCon _ ) = False
  hnf (TAp a _) = hnf a
  hnf (TGen _ ) = error "Unexpected TGen in instance predicate"

-- Predicates not in HNF are broken down using byInst. If this fails, then the
-- predicate is unsatisfiable.
toHnfs :: ClassEnv -> [Pred] -> TI [Pred]
toHnfs ce ps = do
  pss <- mapM (toHnf ce) ps
  pure (concat pss)

toHnf :: ClassEnv -> Pred -> TI [Pred]
toHnf ce p
  | inHnf p = pure [p]
  | otherwise = case byInst ce p of
    Nothing -> throw (ContextReductionFailed p)
    Just ps -> toHnfs ce ps

-- A predicate p in a list of predicates (p : ps) can be eliminated if ps
-- entails p.
simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
 where
  loop rs [] = rs
  loop rs (p : ps) | entail ce (rs ++ ps) p = loop rs ps
                   | otherwise              = loop (p : rs) ps

-- Context reduction is then a combination of toHnfs and simplify.
-- Note: there is some redundancy in this definition: see p.19 of THIH.
reduce :: ClassEnv -> [Pred] -> TI [Pred]
reduce ce ps = do
  qs <- toHnfs ce ps
  pure (simplify ce qs)


------------------------------------
-- Type Schemes
--   Representing polymorphic types
------------------------------------

data Scheme = Forall [Kind] (Qual Type)
  deriving (Eq, Show)

-- In Forall ks qt, each type of the form TGen n that appears in qt represents a
-- generic type variable whose kind is given by ks !! n.
-- This is the only place where we allow TGen values to appear in a type.

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall _ qt) = tv qt

-- Type schemes are constructed by quantifying a qualified type qt with respect
-- to a list of type variables vs:
quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
 where
  vs' = [ v | v <- tv qt, v `elem` vs ]
  ks  = map kind vs'
  s   = zip vs' (map TGen [0 ..])

-- We sometimes need to convert a type into a scheme without adding any
-- qualifying predicates or quantified variables.
toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

---------------
-- Assumptions
---------------

-- TODO: what are assumptions? what is an example assumtion?

data Assump = Id :>: Scheme
  deriving (Show)

instance Types Assump where
  apply s (i :>: sc) = i :>: apply s sc
  tv (_ :>: sc) = tv sc

-- Find the type of a variable in a set of assumptions
find :: Id -> [Assump] -> TI Scheme
find i [] = throw $ UnboundIdentifier i
find i ((i' :>: sc) : as) | i == i'   = pure sc
                          | otherwise = find i as

----------------------------
-- The Type Inference Monad
----------------------------

-- Our monad needs the following:
-- - to keep track of the current substitution
-- - to generate fresh type variables

-- Errors are represented by the Error type, and signalled by throw

newtype TI a = TI (Subst -> Int -> Either Error (Subst, Int, a))
data Error = UnboundIdentifier Id
           | CannotUnify Type Type
           | OccursCheckFailed Tyvar Type
           | TypesDontMatch Type Type
           | KindsDontMatch Tyvar Type
           | SubstitutionsDontAgree Subst Subst
           | SignatureTooGeneral Scheme Scheme
           | ContextTooWeak [Pred]
           | ContextReductionFailed Pred
           | CannotResolveAmbiguity [Ambiguity]
           | NoClassForInstance Pred
           | OverlappingInstance Pred
           deriving (Show, Eq)

instance Functor TI where
  fmap f (TI g) = TI $ \s n -> fmap (\(s', n', x) -> (s', n', f x)) (g s n)

instance Applicative TI where
  pure x = TI (\s n -> pure (s, n, x))
  liftA2 f (TI x) (TI y) = TI $ \s n -> case x s n of
    Left  err          -> Left err
    Right (s', n', x') -> case y s' n' of
      Left  err            -> Left err
      Right (s'', n'', y') -> Right (s'', n'', f x' y')

instance Monad TI where
  return = pure
  TI f >>= g = TI $ \s n -> do
    (s', n', x) <- f s n
    let TI g' = g x
    g' s' n'

instance MonadError Error TI where
  throwError err = TI $ \_s _n -> Left err
  catchError (TI mx) h = TI $ \s n -> case mx s n of
    Left  err -> let (TI h') = h err in h' s n
    Right x   -> Right x

throw :: (MonadError e m) => e -> m a
throw = throwError

runTI :: TI a -> Either Error a
runTI (TI f) = case f nullSubst 0 of
  Left  err       -> Left err
  Right (_, _, x) -> Right x

debugTI :: TI a -> Either Error (Subst, Int, a)
debugTI (TI f) = f nullSubst 0

-- Get the current substitution
getSubst :: TI Subst
getSubst = TI $ \s n -> pure (s, n, s)

-- Extend the current substitution with another
extSubst :: Subst -> TI ()
extSubst s' = TI $ \s n -> pure (s' @@ s, n, ())

-- Extend the current substitution with the most general unifier of two types
unify :: Type -> Type -> TI ()
unify t1 t2 = do
  s <- getSubst
  u <- mgu (apply s t1) (apply s t2)
  extSubst u

-- Generic a new type variable
newTVar :: Kind -> TI Type
newTVar k = TI $ \s n -> let v = Tyvar (enumId n) k in pure (s, n + 1, TVar v)

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
  ts <- mapM newTVar ks
  pure (inst ts qt)

-- inst ts t replaces each ocurrence of a generic variable TGen n with ts !! n.
class Instantiate t where
  inst :: [Type] -> t -> t
instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n ) = ts !! n
  inst _  t         = t
instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)
instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

-----------------------------
-- Type Inference
--   Where the magic happens
-----------------------------

-- A representation of typing rules
type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

-- Typing literals

data Literal = LitInt Integer
             | LitChar Char
             | LitRat Rational
             | LitFloat Double
             | LitStr String
             deriving Show

tiLit :: Literal -> TI ([Pred], Type)
tiLit (LitChar  _) = pure ([], tChar)
-- Integers are not overloaded (yet)
tiLit (LitInt   _) = pure ([], tInt)
tiLit (LitStr   _) = pure ([], tString)
tiLit (LitFloat _) = pure ([], tFloat)
tiLit (LitRat   _) = do
  v <- newTVar Star
  pure ([IsIn (fractionalClass) v], v)

-- Patterns

-- Patterns are used in lambda abstractions, function bindings, pattern
-- bindings, list comprehensions, do notation and case expressions.

data Pat = PVar Id
         | PWildcard
         | PAs Id Pat
         | PLit Literal
         | PNpk Id Integer
         | PCon Assump [Pat]
         deriving (Show)

tiPat :: Pat -> TI ([Pred], [Assump], Type)
tiPat (PVar i) = do
  v <- newTVar Star
  pure ([], [i :>: toScheme v], v)
tiPat PWildcard = do
  v <- newTVar Star
  pure ([], [], v)
tiPat (PAs i pat) = do
  (ps, as, t) <- tiPat pat
  pure (ps, (i :>: toScheme t) : as, t)
tiPat (PLit l) = do
  (ps, t) <- tiLit l
  pure (ps, [], t)
tiPat (PNpk i _k) = do
  t <- newTVar Star
  pure ([IsIn integralClass t], [i :>: toScheme t], t)
tiPat (PCon (_ :>: sc) pats) = do
  (ps, as, ts) <- tiPats pats
  t'           <- newTVar Star
  (qs :=> t)   <- freshInst sc
  unify t (foldr fn t' ts)
  pure (ps ++ qs, as, t')

tiPats :: [Pat] -> TI ([Pred], [Assump], [Type])
tiPats pats = do
  psasts <- mapM tiPat pats
  let ps = concat [ ps' | (ps', _, _) <- psasts ]
      as = concat [ as' | (_, as', _) <- psasts ]
      ts = [ t | (_, _, t) <- psasts ]
  pure (ps, as, ts)

-- Expressions

data Expr = Var Id
          | Lit Literal
          | Const Assump
          | Ap Expr Expr
          | Let BindGroup Expr
          deriving Show

tiExpr :: Infer Expr Type
tiExpr _ce as (Var i) = do
  sc         <- find i as
  (ps :=> t) <- freshInst sc
  pure (ps, t)
tiExpr _ce _as (Const (_ :>: sc)) = do
  (ps :=> t) <- freshInst sc
  pure (ps, t)
tiExpr _ce _as (Lit l) = do
  (ps, t) <- tiLit l
  pure (ps, t)
tiExpr ce as (Ap e f) = do
  (ps, te) <- tiExpr ce as e
  (qs, tf) <- tiExpr ce as f
  t        <- newTVar Star
  unify (tf `fn` t) te
  pure (ps ++ qs, t)
tiExpr ce as (Let bg e) = do
  (ps, as') <- tiBindGroup ce as bg
  (qs, t  ) <- tiExpr ce (as' ++ as) e
  pure (ps ++ qs, t)

----------------
-- Alternatives
----------------

-- An Alt specifies the LHS and RHS of a function definition.
-- In future it could be used for lambda and case expressions as well.
type Alt = ([Pat], Expr)

tiAlt :: Infer Alt Type
tiAlt ce as (pats, e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t)       <- tiExpr ce (as' ++ as) e
  pure (ps ++ qs, foldr fn t ts)

-- tiAlts infers types for a list of alternatives and checks that each agree
-- with some known type.
-- To have tiAlts determine a type from scratch we just pass in a fresh type
-- variable instead of a concrete type.
tiAlts :: ClassEnv -> [Assump] -> [Alt] -> Type -> TI [Pred]
tiAlts ce as alts t = do
  psts <- mapM (tiAlt ce as) alts
  mapM_ (unify t . snd) psts
  pure (concatMap fst psts)

--------------------------------
-- Generalisation
--   From types to type schemes
--------------------------------

-- split paritions a list of predicates into deferred predicates and retained
-- predicates.
-- TODO: explain this a bit more
split :: ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred] -> TI ([Pred], [Pred])
split ce fs gs ps = do
  ps' <- reduce ce ps
  let (ds, rs) = partition (all (`elem` fs) . tv) ps'
  rs' <- defaultedPreds ce (fs ++ gs) rs
  pure (ds, rs \\ rs')

-- Ambiguity
-- A type scheme ps => t is ambiguous if ps contains generic variables that do
-- not also appear in t. e.g.
--   stringInc :: (Read a, Num a) => String -> String

type Ambiguity = (Tyvar, [Pred])

ambiguities :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities _ce vs ps = [ (v, filter (elem v . tv) ps) | v <- tv ps \\ vs ]

modPrim :: ModuleName
modPrim = ModuleName ["Lam", "Primitive"]

-- TODO: cull some of these classes
numClasses :: [Id]
numClasses = [integralClass, fractionalClass, numClass]

ordClass :: Id
ordClass = TopLevel modPrim "Ord"
eqClass :: Id
eqClass = TopLevel modPrim "Eq"
showClass :: Id
showClass = TopLevel modPrim "Show"
fractionalClass :: Id
fractionalClass = TopLevel modPrim "Fractional"
integralClass :: Id
integralClass = TopLevel modPrim "Integral"
numClass :: Id
numClass = TopLevel modPrim "Num"

stdClasses :: [Id]
stdClasses =
  numClasses
    ++ [eqClass, ordClass, showClass, fractionalClass, integralClass]
    ++ map (TopLevel modPrim)
           ["Read", "Bounded", "Enum", "Ix", "Functor", "Monad", "MonadPlus"]

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) =
  [ t'
  | let is = [ i | IsIn i t <- qs ]
        ts = [ t | IsIn i t <- qs ]
  , all (TVar v ==) ts
  , any (`elem` numClasses) is
  , all (`elem` stdClasses) is
  , t' <- defaults ce
  , all (entail ce []) [ IsIn i t' | i <- is ]
  ]

withDefaults
  :: ([Ambiguity] -> [Type] -> a) -> ClassEnv -> [Tyvar] -> [Pred] -> TI a
withDefaults f ce vs ps | any null tss = throw (CannotResolveAmbiguity vps)
                        | otherwise    = pure (f vps (map head tss))
 where
  vps = ambiguities ce vs ps
  tss = map (candidates ce) vps

defaultedPreds :: ClassEnv -> [Tyvar] -> [Pred] -> TI [Pred]
defaultedPreds = withDefaults (\vps ts -> concatMap snd vps)

defaultSubst :: ClassEnv -> [Tyvar] -> [Pred] -> TI Subst
defaultSubst = withDefaults (\vps ts -> zip (map fst vps) ts)

------------------
-- Binding Groups
------------------

-- Note:
-- The interaction between implicitly and explicitly typed bindings leads to a
-- lot of additional complexity. For Lam, we might gain a lot by forcing all top
-- level bindings to have a type signature. The question is whether this is too
-- onerous for users who aren't sure what the type of thei binding is and would
-- like the compiler to infer it.
-- Can we get away with special-casing the inference of top level bindings as
-- part of showing an error message about them? Would that simplify things
-- whilst still retaining good ergonomics?

-- Explicitly typed bindings

type Expl = (Id, Scheme, [Alt])

tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExpl ce as (i, sc, alts) = do
  (qs :=> t) <- freshInst sc
  ps         <- tiAlts ce as alts t
  s          <- getSubst
  let qs' = apply s qs
      t'  = apply s t
      fs  = tv (apply s as)
      gs  = tv t' \\ fs
      sc' = quantify gs (qs' :=> t')
      ps' = filter (not . entail ce qs') (apply s ps)
  (ds, rs) <- split ce fs gs ps'
  if sc /= sc'
    then throw (SignatureTooGeneral sc sc')
    else if not (null rs) then throw (ContextTooWeak rs) else pure ds

-- Implicitly typed bindings
-- There are two complications:
-- - we must deal with groups of mutually recursive bindings as a single unit
-- rather than inferring types for each binding one at a time
-- - the monomorphism restriction can kick in, restricting the use of
-- overloading in certain cases.

type Impl = (Id, [Alt])

-- The monomorphism restriction is invoked when one or more of the entries in a
-- list of implicitly typed bindings is simple, meaning it has an alternative
-- with no LHS patterns.

restricted :: [Impl] -> Bool
restricted = any simple where simple (i, alts) = any (null . fst) alts

tiImpls :: Infer [Impl] [Assump]
tiImpls ce as bs = do
  ts <- mapM (const (newTVar Star)) bs
  let is    = map fst bs
      scs   = map toScheme ts
      as'   = zipWith (:>:) is scs ++ as
      altss = map snd bs
  pss <- zipWithM (tiAlts ce as') altss ts
  s   <- getSubst
  let ps' = apply s (concat pss)
      ts' = apply s ts
      fs  = tv (apply s as)
      vss = map tv ts'
      gs  = foldr1 union vss \\ fs
  (ds, rs) <- split ce fs (foldr1 intersect vss) ps'
  if restricted bs
    then
      let gs'  = gs \\ tv rs
          scs' = map (quantify gs' . ([] :=>)) ts'
      in  pure (ds ++ rs, zipWith (:>:) is scs')
    else
      let scs' = map (quantify gs . (rs :=>)) ts'
      in  pure (ds, zipWith (:>:) is scs')

-- Combined binding groups

-- The first component of a binding group lists any explicitly typed bindings in
-- the group.
-- The second component provides an opportunity to break down the list of any
-- implicitly typed bindings into several smaller lists, arranged in dependency
-- order.
-- Any implicitly typed binding should depend only on the explicitly typed
-- bindings and the implicitly typed bindings that appear before it in the list.
type BindGroup = ([Expl], [[Impl]])

-- There are many subtleties in how binding groups are treated. See p35.

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce as (es, iss) = do
  let as' = [ v :>: sc | (v, sc, _alts) <- es ]
  (ps, as'') <- tiSeq tiImpls ce (as' ++ as) iss
  qss        <- mapM (tiExpl ce (as'' ++ as' ++ as)) es
  pure (ps ++ concat qss, as'' ++ as')

tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq _ti _ce _as []         = pure ([], [])
tiSeq ti  ce  as  (bs : bss) = do
  (ps, as' ) <- ti ce as bs
  (qs, as'') <- tiSeq ti ce (as' ++ as) bss
  pure (ps ++ qs, as'' ++ as')


----------------------------
-- Top level binding groups
----------------------------

type Program = [BindGroup]

tiProgram :: ClassEnv -> [Assump] -> Program -> Either Error [Assump]
tiProgram ce as bgs = runTI $ do
  (ps, as') <- tiSeq tiBindGroup ce as bgs
  s         <- getSubst
  rs        <- reduce ce (apply s ps)
  s'        <- defaultSubst ce [] rs
  pure $ apply (s' @@ s) as'
