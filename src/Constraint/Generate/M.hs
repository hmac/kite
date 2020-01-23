{-# LANGUAGE FlexibleInstances #-}
module Constraint.Generate.M where

-- The constraint generation monad, consisting of:
-- - A State part, generate fresh variables (like NameGen)
-- - A Writer part, to log new variables to a set for use in solving

import qualified Data.Set                      as Set
import           Data.Map.Strict                ( Map )
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

import           Constraint
import           Constraint.Expr                ( Scheme )
import           Data.Name

type Env = Map RawName Scheme

instance Sub Env where
  sub s = fmap (sub s)

type GenerateM = WriterT (Set.Set Var) (State Int)

-- Generate a fresh rigid type variable
freshR :: GenerateM Var
freshR = do
  k <- get
  put (k + 1)
  let var = R (Name ("$R" <> show k))
  tell (Set.singleton var)
  pure var

-- Generate a fresh unification type variable
fresh :: GenerateM Var
fresh = do
  k <- get
  put (k + 1)
  let var = U (Name (show k))
  tell (Set.singleton var)
  pure var

run :: GenerateM a -> (a, Set.Set Var)
run m = evalState (runWriterT m) 0

-- Converts a -> b -> c into [a, b, c]
unfoldFnType :: Type -> [Type]
unfoldFnType (TCon "->" [x, y]) = x : unfoldFnType y
unfoldFnType t                  = [t]

mkTupleType :: [Type] -> Type
mkTupleType args = TCon name args
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
