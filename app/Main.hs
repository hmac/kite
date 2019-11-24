module Main where

import           Syntax
import           Parse
import           Text.Pretty.Simple             ( pPrint )

-- Parse stdin as a Lam module and print the result
main :: IO ()
main = do
  input <- getContents
  case parseLamFile input of
    Left  e -> putStrLn e
    Right r -> pPrint r
