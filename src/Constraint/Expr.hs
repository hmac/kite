module Constraint.Expr where

import           Data.Set                       ( (\\) )
import qualified Data.Set                      as Set

import           Constraint
import           Canonical                      ( Name(..) )
import           Util

import           Syntax                         ( Pattern_ )

-- An example syntax type that we'll eventually replace with something linked to
-- Syn.
data Exp = Var Name
         | Con Con
         | App Exp Exp
         | Abs [Name] Exp
         | Case Exp [Alt]
         | Let [(Name, Exp)] Exp
         | LetA Name Scheme Exp Exp
         | Hole Name
         | TupleLit [Exp]
         | ListLit [Exp]
         | IntLit Int
         | StringLit String [(Exp, String)]
         deriving (Eq, Show)

-- Exp with type annotation
data ExpT = VarT Name Type
          | ConT Con
          | AppT ExpT ExpT
          | AbsT [(Name, Type)] ExpT
          | CaseT ExpT [AltT] Type
          | LetT [(Name, ExpT)] ExpT Type
          | LetAT Name Scheme ExpT ExpT Type
          | HoleT Name Type
          | TupleLitT [ExpT] Type
          | ListLitT [ExpT] Type
          | IntLitT Int Type
          | StringLitT String [(ExpT, String)] Type
         deriving (Eq, Show)

instance Sub ExpT where
  sub s (VarT n v      ) = VarT n (sub s v)
  sub _ (ConT c        ) = ConT c
  sub s (AppT a     b  ) = AppT (sub s a) (sub s b)
  sub s (AbsT binds e  ) = AbsT (mapSnd (sub s) binds) (sub s e)
  sub s (CaseT e alts t) = CaseT (sub s e) (map (sub s) alts) (sub s t)
  sub s (LetT binds body t) =
    LetT (mapSnd (sub s) binds) (sub s body) (sub s t)
  sub s (LetAT x sch e b t) = LetAT x sch (sub s e) (sub s b) (sub s t)
  sub s (HoleT     n  t   ) = HoleT n (sub s t)
  sub s (TupleLitT es t   ) = TupleLitT (map (sub s) es) (sub s t)
  sub s (ListLitT  es t   ) = ListLitT (map (sub s) es) (sub s t)
  sub s (IntLitT   i  t   ) = IntLitT i (sub s t)
  sub s (StringLitT p cs t) = StringLitT p (mapFst (sub s) cs) (sub s t)

data Alt = Alt Pattern Exp
  deriving (Eq, Show)

data AltT = AltT Pattern ExpT
  deriving (Eq, Show)

type Pattern = Pattern_ Name

instance Sub AltT where
  sub s (AltT p e) = AltT p (sub s e)

-- Note: raw data constructors have the following type (a Scheme):
-- Forall [a, b, ..] [] t
-- where t has the form m -> n -> ... -> T a b ..

type Con = Name

-- The Var will always be rigid type variables (I think)
data Scheme = Forall [Var] Constraint Type
  deriving (Eq, Show)

instance Sub Scheme where
  sub s (Forall vars c t) =
    let s' = filter (\(v, _) -> v `notElem` vars) s
    in  Forall vars (sub s' c) (sub s' t)

instance Vars Scheme where
  fuv (Forall tvars c t) = fuv c <> fuv t \\ Set.fromList tvars
