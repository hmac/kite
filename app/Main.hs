module Main where

import           Parse
import           Print

import           Text.Pretty.Simple             ( pPrint )
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Prettyprint.Doc
import           System.IO                      ( stdout )

import           Desugar                        ( desugarModule )
import           Translate                      ( tiModule )
import           THIH                           ( Error(..) )
import qualified Repl                           ( run )
import           ELC                            ( translateModule
                                                , primConstructors
                                                )
import           LC                             ( runConvert
                                                , convertEnv
                                                )
import           EvalLC                         ( evalMain )
import           System.Environment             ( getArgs )

-- Parse stdin as a Lam module and pretty print the result
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["repl"]      -> Repl.run
    ["run", file] -> run file

run :: FilePath -> IO ()
run path = do
  input <- readFile path
  case parseLamFile input of
    Left  e -> putStrLn e
    Right m -> do
      let core = desugarModule m
      case tiModule core of
        Left  (Error err) -> putStrLn err
        Right _           -> do
          let env =
                runConvert (translateModule primConstructors m >>= convertEnv)
          let answer = evalMain env
          pPrint answer

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
