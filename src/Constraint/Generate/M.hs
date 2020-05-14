{-# LANGUAGE FlexibleInstances #-}
module Constraint.Generate.M
  ( module Constraint.Generate.M
  , throwError
  )
where

-- The constraint generation monad, consisting of:
-- - A State part, generate fresh variables (like NameGen)
-- - A Writer part, to log new variables to a set for use in solving

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map.Strict                ( Map )
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Control.Monad.Except

import           Constraint
import           Data.Name
import           Util

type TypeEnv = Map Name Scheme

-- @e@ is the type of error
type GenerateM e = ExceptT e (WriterT (Set Var) (State Int))

-- Generate a fresh rigid type variable
freshR :: GenerateM e Var
freshR = do
  k <- get
  put (k + 1)
  let var = R (Local (Name ("$R" <> show k)))
  tell (Set.singleton var)
  pure var

-- Generate a fresh unification type variable
fresh :: GenerateM e Var
fresh = do
  k <- get
  put (k + 1)
  let var = U (Local (Name (show k)))
  tell (Set.singleton var)
  pure var

run :: GenerateM e a -> (Either e a, Set Var)
run m = evalState (runWriterT (runExceptT m)) 0

-- | Transform the error type of a GenerateM
mapError :: (e -> e') -> GenerateM e a -> GenerateM e' a
mapError = mapExceptT . fmap . first
