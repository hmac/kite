module Constraint.GenerateM where

-- The constraint generation monad, consisting of:
-- - A State part, generate fresh variables (like NameGen)
-- - A Writer part, to log new variables to a set for use in solving

import qualified Data.Set                      as Set
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

import           Constraint
import           Data.Name

type GenerateM = WriterT (Set.Set Var) (State Int)

fresh :: GenerateM Var
fresh = do
  k <- get
  put (k + 1)
  let var = U (Name (show k))
  tell (Set.singleton var)
  pure var

run :: GenerateM a -> (a, Set.Set Var)
run m = evalState (runWriterT m) 0
