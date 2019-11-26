module Main where

import           Parse
import           Print

-- Parse stdin as a Lam module and pretty print the result
main :: IO ()
main = do
  input <- getContents
  case parseLamFile input of
    Left  e -> putStrLn e
    Right r -> print (printModule r)
