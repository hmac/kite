module ELC where

-- The Enriched Lambda Calculus
-- Taken from The Implementation of Functional Programming Languages, Chapter 3

import           Data.Name                      ( Name(..) )
import           Data.Map.Strict                ( Map )

data Exp = Const Constant [Exp]
         | Var Name
         | Cons Con [Exp]
         | App Exp Exp
         | Abs Pattern Exp
         | Let Pattern Exp Exp
         | LetRec [(Pattern, Exp)] Exp
         | Fatbar Exp Exp
         | If Exp Exp Exp
         | Case Name [Clause]
         | Fail
         | Bottom String
         | Project Int Int Exp   -- arity of the constructor; index of the field
         | Y Exp                 -- the Y combinator
         | Record (Map String Exp)
         | RecordProject Exp String
         deriving (Show, Eq)

buildAbs :: Exp -> [Pattern] -> Exp
buildAbs = foldr Abs

buildApp :: Exp -> [Exp] -> Exp
buildApp = foldl App

data Clause = Clause Con [Name] Exp
            deriving (Eq, Show)

data Constant = Int Int
              | String String
              | Bool Bool
              | Prim Primitive
         deriving (Show, Eq)

-- TODO: add PrimShowInt etc.
data Primitive = PrimStringAppend
               | PrimAdd
               | PrimSub
               | PrimMult
               | PrimShow -- TODO: this becomes PrimShowInt etc when we have a show typeclass
               | PrimShowInt
               | PrimEqInt
         deriving (Show, Eq)

data Pattern = ConstPat Constant
             | VarPat Name
             | ConPat Con [Pattern]
         deriving (Show, Eq)

data Con = Prod { conName :: Name, conArity :: Int }
         | Sum { conName :: Name, conArity :: Int, sumTag :: Int, sumFamily :: [Con] }

-- Because Sums contain infinite loops via family, we need to manually write Eq
-- and Show instances.
instance Eq Con where
  Prod { conName = n, conArity = a } == Prod { conName = n', conArity = a' } =
    n == n' && a == a'
  Sum { conName = n, conArity = a, sumTag = t } == Sum { conName = n', conArity = a', sumTag = t' }
    = n == n' && t == t' && a == a'
  _ == _ = False
instance Show Con where
  show Prod { conName = n, conArity = a } =
    "Prod { name = " <> show n <> ", arity = " <> show a <> " }"
  show Sum { conName = n, conArity = a, sumTag = t } =
    "Sum { name = "
      <> show n
      <> ", tag = "
      <> show t
      <> ", arity = "
      <> show a
      <> " }"
