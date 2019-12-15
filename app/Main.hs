{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Syn.Parse
import           Syn.Print

import           Text.Pretty.Simple             ( pPrint )
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Prettyprint.Doc
import           System.IO                      ( stdout )

import qualified ModuleLoader
import           ModuleGraphTypechecker         ( typecheckModule )
import           ModuleGraphCompiler            ( compileModule
                                                , CompiledModule(..)
                                                )

import           Typecheck.Desugar              ( desugarModule )
import           Typecheck.Translate            ( tiModule )
import           Typecheck.Error                ( printError )
import qualified Repl                           ( run )
import qualified ELC.Compile                   as ELC
import           LC.Compile                     ( runConvert
                                                , convertEnv
                                                )
import qualified LC.Print                       ( print )
import           LC.Eval                        ( evalMain )
import           Syntax                         ( Module
                                                , Syn
                                                )
import qualified Canonicalise                  as Can
import           Options.Generic

data Config =
      Repl
    | Run FilePath
    | Typecheck FilePath
    | DumpParse FilePath
    | DumpElc FilePath
    | DumpLc FilePath
    deriving (Generic, Show)

instance ParseRecord Config

-- Parse stdin as a Lam module and pretty print the result
main :: IO ()
main = do
  cfg <- getRecord "lam"
  case cfg of
    Repl        -> Repl.run
    Run       f -> run f
    DumpParse f -> dumpParse f
    DumpElc   f -> dumpELC f
    DumpLc    f -> dumpLC f
    Typecheck f -> typecheck f

dumpParse :: FilePath -> IO ()
dumpParse path = do
  input <- readFile path
  case parseLamFile input of
    Left  e -> putStrLn e
    Right m -> pPrint m

dumpELC :: FilePath -> IO ()
dumpELC = withParsedFile $ \m -> pPrint
  $ runConvert (ELC.translateModule ELC.defaultEnv (Can.canonicaliseModule m))

dumpLC :: FilePath -> IO ()
dumpLC = withParsedFile $ \m -> pPrint $ runConvert
  (ELC.translateModule ELC.defaultEnv (Can.canonicaliseModule m) >>= convertEnv)

typecheck :: FilePath -> IO ()
typecheck = withParsedFile $ \m -> case tiModule (desugarModule m) of
  Left  err -> putStrLn (printError err)
  Right _   -> putStrLn "Success."

run :: FilePath -> IO ()
run path = do
  mod <- ModuleLoader.loadFromPath path
  case mod of
    Left  err -> putStrLn err
    Right l   -> case typecheckModule l of
      Left err -> putStrLn (printError err)
      Right _ ->
        let cm     = compileModule l
            answer = evalMain (cModuleName cm) (cModuleEnv cm)
        in  renderIO stdout (layout (LC.Print.print answer)) >> putStrLn ""

withParsedFile :: (Module Syn -> IO ()) -> FilePath -> IO ()
withParsedFile cb path = do
  input <- readFile path
  case parseLamFile input of
    Left  e -> putStrLn e
    Right m -> cb m

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
