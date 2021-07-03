module NameGen
  ( NameGen
  , freshM
  , run
  ) where

import           Control.Monad.State.Strict

type NameGen m = StateT Int m

freshM :: Monad m => (Int -> a) -> NameGen m a
freshM f = do
  k <- get
  put (k + 1)
  pure (f k)

run :: Monad m => NameGen m a -> m a
run m = evalStateT m 0
