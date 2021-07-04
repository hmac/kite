module Main where

import           Control.Monad                  ( unless )
import           Language.Haskell.HLint
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )

-- Run hlint on all source directories.
-- -XNoPatternSynonyms is to work around a bug in hlint where it can't parse code with
-- variables called 'pattern' (https://github.com/ndmitchell/hlint/issues/216)
main :: IO ()
main = do
  args  <- getArgs
  hints <-
    hlint $ ["-XNoPatternSynonyms", "src", "test", "app", "benchmarks"] ++ args
  unless (null hints) exitFailure
