module Main where

import           Parse
import           Print

import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Prettyprint.Doc
import           System.IO                      ( stdout )

-- Parse stdin as a Lam module and pretty print the result
main :: IO ()
main = do
  input <- getContents
  case parseLamFile input of
    Left  e -> putStrLn e
    Right r -> renderIO stdout (layout (printModule r)) >> putStrLn ""

layout :: Document -> SimpleDocStream AnsiStyle
layout doc = reAnnotateS styleToColor (layoutPretty defaultLayoutOptions doc)

-- Conor Colours
-- https://github.com/idris-lang/Idris-dev/blob/master/docs/reference/semantic-highlighting.rst
styleToColor :: Style -> AnsiStyle
styleToColor VarStyle      = color Magenta
styleToColor KeywordStyle  = bold
styleToColor FunctionStyle = color Green
styleToColor TypeStyle     = color Blue
styleToColor DataStyle     = color Red
styleToColor HoleStyle     = color Magenta <> italicized
