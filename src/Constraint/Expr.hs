module Constraint.Expr where

import           Data.Set                       ( (\\) )
import qualified Data.Set                      as Set

import           Constraint
import           Data.Name
import           Util

-- An example syntax type that we'll eventually replace with something linked to
-- Syn.
data Exp = Var RawName
         | Con Con
         | App Exp Exp
         | Abs RawName Exp
         | Case Exp [Alt]
         | Let RawName Exp Exp
         | LetA RawName Scheme Exp Exp
         | Hole RawName
         | TupleLit [Exp]
         | ListLit [Exp]
         | IntLit Int
         | StringLit String [(Exp, String)]
         deriving (Eq, Show)

-- Exp with type annotation
data ExpT = VarT RawName Type
          | ConT Con
          | AppT ExpT ExpT
          | AbsT RawName Type ExpT
          | CaseT ExpT [AltT] Type
          | LetT RawName ExpT ExpT Type
          | LetAT RawName Scheme ExpT ExpT Type
          | HoleT RawName Type
          | TupleLitT [ExpT] Type
          | ListLitT [ExpT] Type
          | IntLitT Int Type
          | StringLitT String [(ExpT, String)] Type
         deriving (Eq, Show)

instance Sub ExpT where
  sub s (VarT n v         ) = VarT n (sub s v)
  sub _ (ConT c           ) = ConT c
  sub s (AppT a b         ) = AppT (sub s a) (sub s b)
  sub s (AbsT  x t    e   ) = AbsT x (sub s t) (sub s e)
  sub s (CaseT e alts t   ) = CaseT (sub s e) (map (sub s) alts) (sub s t)
  sub s (LetT x e b t     ) = LetT x (sub s e) (sub s b) (sub s t)
  sub s (LetAT x sch e b t) = LetAT x sch (sub s e) (sub s b) (sub s t)
  sub s (HoleT     n  t   ) = HoleT n (sub s t)
  sub s (TupleLitT es t   ) = TupleLitT (map (sub s) es) (sub s t)
  sub s (ListLitT  es t   ) = ListLitT (map (sub s) es) (sub s t)
  sub s (IntLitT   i  t   ) = IntLitT i (sub s t)
  sub s (StringLitT p cs t) = StringLitT p (mapFst (sub s) cs) (sub s t)

-- [RawName] are the variables bound by the case branch
data Alt = Alt Pat Exp
  deriving (Eq, Show)

data AltT = AltT Pat ExpT
  deriving (Eq, Show)

instance Sub AltT where
  sub s (AltT p e) = AltT p (sub s e)

data Pat = ConPat Con [Pat]   -- T a b c
         | VarPat RawName     -- a
         | WildPat            -- _
         | IntPat Int         -- 5
         | TuplePat [Pat]     -- (x, y)
         | ListPat [Pat]      -- [x, y]
         | StringPat String   -- "hi"
        deriving (Eq, Show)

-- A subset of patterns which are not inductive.
-- Currently used in case alternatives but we'll replace them with full patterns
-- soon.
data SimplePat = SConPat Con [RawName]   -- T a b c
               | SVarPat RawName         -- a
               | SWildPat                -- _
               deriving (Eq, Show)

-- Note: raw data constructors have the following type (a Scheme):
-- Forall [a, b, ..] [] t
-- where t has the form m -> n -> ... -> T a b ..

newtype Con = C RawName
  deriving (Eq, Show)

-- The Var will always be rigid type variables (I think)
data Scheme = Forall [Var] Constraint Type
  deriving (Eq, Show)

instance Sub Scheme where
  sub s (Forall vars c t) =
    let s' = filter (\(v, _) -> v `notElem` vars) s
    in  Forall vars (sub s' c) (sub s' t)

instance Vars Scheme where
  fuv (Forall tvars c t) = fuv c <> fuv t \\ Set.fromList tvars
