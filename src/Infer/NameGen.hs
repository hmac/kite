module Infer.NameGen
  ( Gen(..)
  , NameGen
  , genName
  , evalNameGen
  )
where

import           Data.Char                      ( chr )
import           Control.Monad.Trans.State.Strict
import           Data.HashSet                   ( HashSet )
import           Data.HashSet                  as Set
import           Syntax                         ( Name(..) )

type NameGen = State Gen

data Gen = Gen Int (HashSet Name)

evalNameGen :: NameGen a -> Gen -> a
evalNameGen = evalState

genName :: NameGen Name
genName = do
  Gen _ used <- get
  name       <- Name <$> genName'
  if Set.member name used then genName else pure name

genName' :: NameGen String
genName' = do
  Gen i used <- get
  put $ Gen (i + 1) used
  pure (gen i)

gen :: Int -> String
gen i | i <= 25   = [chr (97 + i)]
      | otherwise = 'z' : gen (i - 26)
