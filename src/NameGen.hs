module NameGen
  ( NameGen
  , freshM
  , run
  )
where

import           Control.Monad.State.Strict

type NameGen = State Int

freshM :: (Int -> a) -> NameGen a
freshM f = do
  k <- get
  put (k + 1)
  pure (f k)

run :: NameGen a -> a
run m = evalState m 0
