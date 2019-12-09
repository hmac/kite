module Main where

import           Parse
import           Print

import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Prettyprint.Doc
import           System.IO                      ( stdout )

import           Desugar                        ( desugarModule )
import           Translate                      ( tiModule )
import           THIH                           ( Error(..) )
import           Repl                           ( run )

-- Parse stdin as a Lam module and pretty print the result
main :: IO ()
main = run

main' = do
  input <- getContents
  case parseLamFile input of
    Left  e -> putStrLn e
    Right m -> do
      let core = desugarModule m
      case tiModule core of
        Left (Error err) -> putStrLn err
        Right _ -> renderIO stdout (layout (printModule m)) >> putStrLn ""

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
