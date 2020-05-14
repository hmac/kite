{-# LANGUAGE FlexibleInstances #-}
-- | The model for typing constraints

module Constraint
  ( Constraint(..)
  , CConstraint(..)
  , Axiom(..)
  , AxiomScheme
  , Type(..)
  , unfoldFnType
  , Var(..)
  , Vars(..)
  , Subst
  , simple
  , implic
  , fn
  , list
  , Sub(..)
  , flattenConstraint
  , partitionConstraint
  , mapConstraint
  , sortConstraint
  , modPrim
  , mkTupleType
  , Error(..)
  , LocatedError(..)
  , Scheme
  , Scheme_(..)
  )
where

import           Util
import           Data.Name
import qualified Data.Set                      as Set
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Syn                            ( Scheme_(..) )

-- Simple constraints
-- Created by the user via type annotations
data Constraint =
                -- | The empty constraint
                  CNil
                -- | Conjuction of two constraints
                | Constraint :^: Constraint
                -- | Equality between two types
                | Type :~: Type
                -- | Typeclass instance
                | Inst Name [Type]
                -- | Record field constraint
                -- HasField R l T means type R must unify with a record type
                -- containing a field with label l and type T
                | HasField Type Name Type
                deriving (Eq, Show, Ord)

-- Top level axiom scheme
data Axiom = AForall (Set Var) Constraint Constraint deriving (Eq, Ord, Show)
type AxiomScheme = [Axiom]

instance Semigroup Constraint where
  CNil      <> c    = c
  c         <> CNil = c
-- Note: this case ensures that <> is associative, but may be inefficient
  (c :^: d) <> e    = c <> (d <> e)
  c         <> d    = c :^: d

instance Monoid Constraint where
  mempty = CNil

type Scheme = Scheme_ Var Constraint Type

flattenConstraint :: Constraint -> [Constraint]
flattenConstraint (c :^: d) = flattenConstraint c <> flattenConstraint d
flattenConstraint CNil      = []
flattenConstraint c         = [c]

-- Like partition but works on Constraints
partitionConstraint
  :: (Constraint -> Bool) -> Constraint -> (Constraint, Constraint)
partitionConstraint f = bimap mconcat mconcat . partition f . flattenConstraint

-- Like map but for Constraints
mapConstraint :: (Constraint -> b) -> Constraint -> [b]
mapConstraint f = map f . flattenConstraint

sortConstraint :: Constraint -> Constraint
sortConstraint = mconcat . sort . flattenConstraint

-- Computed constraints
-- Generated by the typechecker
data CConstraint = Simple Constraint
                | CConstraint :^^: CConstraint
                -- these are always unification vars
                | E [Var] Constraint CConstraint
                deriving (Eq, Show, Ord)

instance Semigroup CConstraint where
  Simple CNil <> c           = c
  c           <> Simple CNil = c
-- Note: this case ensures that <> is associative, but may be inefficient
  (c :^^: d)  <> e           = c <> (d <> e)
  c           <> d           = c :^^: d

instance Monoid CConstraint where
  mempty = Simple mempty

data Type = TVar Var
          | TCon Name
          | TApp Type Type
          | THole Name
          | TInt
          | TString
          | TRecord [(Name, Type)]
          deriving (Eq, Show, Ord)

modPrim :: ModuleName
modPrim = "Lam.Primitive"

infixr 4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TApp (TApp (TCon (TopLevel modPrim "->")) a) b

list :: Type -> Type
list = TApp (TCon (TopLevel modPrim "List"))

mkTupleType :: [Type] -> Type
mkTupleType args = foldl TApp (TCon (TopLevel modPrim name)) args
 where
  name = case length args of
    0 -> "Unit"
    2 -> "Tuple2"
    3 -> "Tuple3"
    4 -> "Tuple4"
    5 -> "Tuple5"
    6 -> "Tuple6"
    7 -> "Tuple7"
    n -> error $ "Unsupported tuple length: " <> show n

-- | Converts a -> b -> c into [a, b, c]
unfoldFnType :: Type -> [Type]
unfoldFnType (TApp (TApp (TCon (TopLevel "Lam.Primitive" "->")) a) b) =
  a : unfoldFnType b
unfoldFnType t = [t]

data Var = R Name
         | U Name
         deriving (Eq, Show, Ord)

-- The type of substitutions
-- TODO: use Map
type Subst = [(Var, Type)]

-- A typeclass for applying substitutions
class Sub a where
  sub :: Subst -> a -> a

instance Sub Type where
  sub s (TVar v  )       = fromMaybe (TVar v) (lookup v s)
  sub _ (TCon n  )       = TCon n
  sub s (TApp a b)       = TApp (sub s a) (sub s b)
  sub _ (THole n )       = THole n
  sub _ TInt             = TInt
  sub _ TString          = TString
  sub s (TRecord fields) = TRecord $ mapSnd (sub s) fields

instance Sub Constraint where
  sub _ CNil                 = CNil
  sub s (a    :^:       b  ) = sub s a :^: sub s b
  sub s (t    :~:       v  ) = sub s t :~: sub s v
  sub s (Inst classname tys) = Inst classname (sub s tys)
  sub s (HasField r l t    ) = HasField (sub s r) l (sub s t)

instance Sub CConstraint where
  sub s (Simple c) = Simple (sub s c)
  sub s (c :^^: d) = sub s c :^^: sub s d
-- not worrying about var capture here because unification vars are unique.
-- this could be problematic, be aware
  sub s (E vs q c) = E vs (sub s q) (sub s c)

instance Sub a => Sub [a] where
  sub s = map (sub s)

instance Sub Scheme where
  sub s (Forall vars c t) =
    let s' = filter (\(v, _) -> v `notElem` vars) s
    in  Forall vars (sub s' c) (sub s' t)

instance Sub (Map Name Scheme) where
  sub s = fmap (sub s)

-- Vars is defined for any type for which we can extract a set of free
-- unification variables
class Vars a where
  -- Get the free unification variables from `a`
  fuv  :: a -> Set Var
  -- Get all free variables from `a` (rigid and unification)
  ftv  :: a -> Set Var

instance Vars Type where
  fuv (TVar (U v))     = Set.singleton (U v)
  fuv (TVar _    )     = mempty
  fuv (TCon _    )     = mempty
  fuv (TApp a b  )     = fuv a <> fuv b
  fuv (THole _   )     = mempty
  fuv TInt             = mempty
  fuv TString          = mempty
  fuv (TRecord fields) = Set.unions (map (fuv . snd) fields)

  ftv (TVar v  )       = Set.singleton v
  ftv (TCon _  )       = mempty
  ftv (TApp a b)       = ftv a <> ftv b
  ftv (THole _ )       = mempty
  ftv TInt             = mempty
  ftv TString          = mempty
  ftv (TRecord fields) = Set.unions (map (ftv . snd) fields)

instance Vars Constraint where
  fuv CNil             = mempty
  fuv (a    :^: b    ) = fuv a <> fuv b
  fuv (t    :~: v    ) = fuv t <> fuv v
  fuv (Inst _   tys  ) = fuv tys
  fuv (HasField r _ t) = fuv r <> fuv t

  ftv CNil             = mempty
  ftv (a    :^: b    ) = ftv a <> ftv b
  ftv (t    :~: v    ) = ftv t <> ftv v
  ftv (Inst _   tys  ) = ftv tys
  ftv (HasField r _ t) = ftv r <> ftv t

instance Vars CConstraint where
  fuv (Simple c   ) = fuv c
  fuv (a :^^: b   ) = fuv a <> fuv b
  fuv (E vars c cc) = fuv c <> fuv cc \\ Set.fromList vars

  ftv (Simple c   ) = ftv c
  ftv (a :^^: b   ) = ftv a <> ftv b
  ftv (E vars c cc) = ftv c <> ftv cc \\ Set.fromList vars

instance Vars b => Vars (Map a b) where
  fuv env = Set.unions (map fuv (Map.elems env))
  ftv env = Set.unions (map ftv (Map.elems env))

instance Vars a => Vars [a] where
  fuv = mconcat . map fuv
  ftv = mconcat . map ftv

instance Vars Scheme where
  fuv (Forall tvars c t) = fuv c <> fuv t \\ Set.fromList tvars
  ftv (Forall tvars c t) = ftv c <> ftv t \\ Set.fromList tvars

simple :: CConstraint -> Constraint
simple E{}        = mempty
simple (Simple c) = c
simple (c :^^: d) = simple c <> simple d

implic :: CConstraint -> [(Set Var, Constraint, CConstraint)]
implic (Simple _  ) = mempty
implic (E vars q c) = [(Set.fromList vars, q, c)]
implic (c :^^: d  ) = implic c <> implic d

data Error = OccursCheckFailure Type Type
           | ConstructorMismatch Type Type
           | UnsolvedConstraints Constraint
           | EquationsHaveDifferentNumberOfPatterns
           | UnsolvedUnificationVariables (Set Var) Constraint
           | UnknownVariable Name
           | EmptyCase
           | DuplicatePatternVariables
           | OverlappingTypeclassInstances
           | UnknownTypeclass Name
           | UnknownInstanceMethod Name
           | RecordDoesNotHaveLabel Type Name
           | ProjectionOfNonRecordType Type Name
  deriving (Show, Eq)

-- An error paired with the name of the binding it originates from.
-- Not as good as tying errors to specific source locations, but better than
-- nothing.
data LocatedError = LocatedError Name Error
  deriving (Eq, Show)
