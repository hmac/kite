{-# LANGUAGE FlexibleInstances #-}
module Constraint.Generate.M
  ( module Constraint.Generate.M
  , throwError
  )
where

-- The constraint generation monad, consisting of:
-- - A State part, generate fresh variables (like NameGen)
-- - A Writer part, to log new variables to a set for use in solving

import qualified Data.Set                      as Set
import           Data.Map.Strict                ( Map )
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Control.Monad.Except

import           Constraint
import           Data.Name

type TypeEnv = Map Name Scheme

type GenerateM = ExceptT Error (WriterT (Set.Set Var) (State Int))

-- Generate a fresh rigid type variable
freshR :: GenerateM Var
freshR = do
  k <- get
  put (k + 1)
  let var = R (Local (Name ("$R" <> show k)))
  tell (Set.singleton var)
  pure var

-- Generate a fresh unification type variable
fresh :: GenerateM Var
fresh = do
  k <- get
  put (k + 1)
  let var = U (Local (Name (show k)))
  tell (Set.singleton var)
  pure var

run :: GenerateM a -> (Either Error a, Set.Set Var)
run m = evalState (runWriterT (runExceptT m)) 0

-- Converts a -> b -> c into [a, b, c]
unfoldFnType :: Type -> [Type]
unfoldFnType (TCon t [x, y]) | t == TopLevel modPrim "->" = x : unfoldFnType y
unfoldFnType t = [t]
