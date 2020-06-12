{-# LANGUAGE FlexibleInstances #-}
-- | The model for typing constraints

module Constraint
  ( Constraint(..)
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
  , modPrim
  , mkTupleType
  , Error(..)
  , LocatedError(..)
  , Scheme
  , Scheme_(..)
  , Constraints
  , TypeEnv
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

-- A list of constraints. Interpreted as a conjunction - i.e. all of them have
-- to be satisfied.
type Constraints = [Constraint]

-- Simple constraints
-- Created by the user via type annotations
data Constraint =
                -- | Equality between two types
                 Type :~: Type
                -- | Record field constraint
                -- HasField R l T means type R must unify with a record type
                -- containing a field with label l and type T
                | HasField Type Name Type
                -- | Existential constraint
                -- This is only used for polymorphic annotations on lets
                -- The [Var] are always unification vars
                -- Unchecked invariant: the first Constraints field contains no
                -- existential constraints.
                | E [Var] Constraints Constraints
                deriving (Eq, Show, Ord)

-- Top level axiom scheme
-- Not currently used, but could hold record field constraints of some sort in
-- the future.
data Axiom = AForall (Set Var) Constraint Constraint deriving (Eq, Ord, Show)
type AxiomScheme = [Axiom]

type Scheme = Scheme_ Var Type

data Type = TVar Var
          | TCon Name
          | TApp Type Type
          | THole Name
          | TInt
          | TString
          | TChar
          | TBool
          | TUnit
          | TRecord [(Name, Type)]
          | TAlias Name Type
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
type Subst = Map Var Type

-- A typeclass for applying substitutions
class Sub a where
  sub :: Subst -> a -> a

instance Sub Type where
  sub s (TVar v  )       = fromMaybe (TVar v) (Map.lookup v s)
  sub _ (TCon n  )       = TCon n
  sub s (TApp a b)       = TApp (sub s a) (sub s b)
  sub _ (THole n )       = THole n
  sub _ TInt             = TInt
  sub _ TString          = TString
  sub _ TChar            = TChar
  sub _ TBool            = TBool
  sub _ TUnit            = TUnit
  sub s (TRecord fields) = TRecord $ mapSnd (sub s) fields
  sub s (TAlias n a    ) = TAlias n (sub s a)

instance Sub Constraint where
  sub s (t :~: v        ) = sub s t :~: sub s v
  sub s (HasField r  l t) = HasField (sub s r) l (sub s t)
  -- not worrying about var capture here because unification vars are unique.
  -- this could be problematic, be aware
  sub s (E        vs q c) = E vs (sub s q) (sub s c)

instance Sub a => Sub [a] where
  sub s = map (sub s)

instance Sub Scheme where
  sub s (Forall vars t) =
    let s' = Map.withoutKeys s (Set.fromList vars) in Forall vars (sub s' t)

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
  fuv TChar            = mempty
  fuv TBool            = mempty
  fuv TUnit            = mempty
  fuv (TRecord fields) = Set.unions (map (fuv . snd) fields)
  fuv (TAlias _ a    ) = fuv a

  ftv (TVar v  )       = Set.singleton v
  ftv (TCon _  )       = mempty
  ftv (TApp a b)       = ftv a <> ftv b
  ftv (THole _ )       = mempty
  ftv TInt             = mempty
  ftv TString          = mempty
  ftv TChar            = mempty
  ftv TBool            = mempty
  ftv TUnit            = mempty
  ftv (TRecord fields) = Set.unions (map (ftv . snd) fields)
  ftv (TAlias _ a    ) = ftv a

instance Vars Constraint where
  fuv (t :~: v           ) = fuv t <> fuv v
  fuv (HasField r    _ t ) = fuv r <> fuv t
  fuv (E        vars c cc) = fuv c <> fuv cc \\ Set.fromList vars

  ftv (t :~: v           ) = ftv t <> ftv v
  ftv (HasField r    _ t ) = ftv r <> ftv t
  ftv (E        vars c cc) = ftv c <> ftv cc \\ Set.fromList vars

instance Vars b => Vars (Map a b) where
  fuv env = Set.unions (map fuv (Map.elems env))
  ftv env = Set.unions (map ftv (Map.elems env))

instance Vars a => Vars [a] where
  fuv = mconcat . map fuv
  ftv = mconcat . map ftv

instance Vars Scheme where
  fuv (Forall tvars t) = fuv t \\ Set.fromList tvars
  ftv (Forall tvars t) = ftv t \\ Set.fromList tvars

-- Filters out any existential implication constraints
simple :: Constraints -> Constraints
simple = mapMaybe $ \case
  E{} -> Nothing
  c   -> Just c

implic :: Constraint -> [(Set Var, Constraints, Constraints)]
implic (E vars qs cs) = [(Set.fromList vars, qs, cs)]
implic _              = mempty

type TypeEnv = Map Name Scheme

data Error = OccursCheckFailure Type Type
           | ConstructorMismatch Type Type
           | UnsolvedConstraints Constraints
           | EquationsHaveDifferentNumberOfPatterns
           | UnsolvedUnificationVariables (Set Var) Constraints
           | UnknownVariable Name
           | EmptyCase
           | DuplicatePatternVariables
           | RecordDoesNotHaveLabel Type Name
           | ProjectionOfNonRecordType Type Name
           -- foreign call name, expected number of args, actual number given
           | WrongNumberOfArgsToForeignCall String Int Int
           -- name of hole, type of hole, in-scope candidates
           | HoleFound Name Type TypeEnv
  deriving (Show, Eq)

-- An error paired with the name of the binding it originates from.
-- Not as good as tying errors to specific source locations, but better than
-- nothing.
data LocatedError = LocatedError Name Error
  deriving (Eq, Show)
