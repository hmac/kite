{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Syn.Parse
import           Syn.Print

import           Text.Pretty.Simple             ( pPrint )
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Prettyprint.Doc
import           System.IO                      ( stdout )

import           ModuleLoader                   ( ModuleGroup(..) )
import           ModuleGroupCompiler            ( CompiledModule(..) )
import qualified ModuleLoader
import qualified ModuleGroupTypechecker
import qualified ModuleGroupCompiler

import           Typecheck.Error                ( printError )
import qualified Repl                           ( run )
import qualified LC.Print                       ( print )
import           LC.Eval                        ( evalMain )
import           Options.Generic

data Config =
      Repl
    | Run FilePath
    | Typecheck FilePath
    | Parse FilePath
    | DumpElc FilePath
    | DumpLc FilePath
    | DumpTypeEnv FilePath
    deriving (Generic, Show)

instance ParseRecord Config

-- Parse stdin as a Lam module and pretty print the result
main :: IO ()
main = do
  cfg <- getRecord "lam"
  case cfg of
    Repl          -> Repl.run
    Run         f -> run f
    Parse       f -> parse f
    DumpElc     f -> dumpELC f
    DumpLc      f -> dumpLC f
    DumpTypeEnv f -> dumpTypeEnv f
    Typecheck   f -> typecheck f

parse :: FilePath -> IO ()
parse path = do
  input <- readFile path
  case parseLamFile input of
    Left  e -> putStrLn e
    Right m -> pPrint m

dumpELC :: FilePath -> IO ()
dumpELC = withParsedFile $ \m -> pPrint $ ModuleGroupCompiler.compileModule' m

dumpLC :: FilePath -> IO ()
dumpLC = withParsedFile $ \m -> pPrint $ ModuleGroupCompiler.compileModule m

dumpTypeEnv :: FilePath -> IO ()
dumpTypeEnv path = do
  modul <- ModuleLoader.loadFromPath path
  case modul of
    Left  err -> putStrLn err
    Right l   -> let env = ModuleGroupTypechecker.dumpEnv l in pPrint env

typecheck :: FilePath -> IO ()
typecheck path = do
  modul <- ModuleLoader.loadFromPath path
  case modul of
    Left  err -> putStrLn err
    Right l   -> case ModuleGroupTypechecker.typecheckModule l of
      Left  err -> putStrLn (printError err)
      Right _   -> putStrLn "Success."

run :: FilePath -> IO ()
run path = do
  modul <- ModuleLoader.loadFromPath path
  case modul of
    Left  err -> putStrLn err
    Right l   -> case ModuleGroupTypechecker.typecheckModule l of
      Left err -> putStrLn (printError err)
      Right _ ->
        let cm     = ModuleGroupCompiler.compileModule l
            answer = evalMain (cModuleName cm) (cModuleEnv cm)
        in  renderIO stdout (layout (LC.Print.print answer)) >> putStrLn ""

withParsedFile :: (ModuleGroup -> IO ()) -> FilePath -> IO ()
withParsedFile cb path = do
  mgroup <- ModuleLoader.loadFromPath path
  case mgroup of
    Left  e -> putStrLn e
    Right g -> cb g

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
