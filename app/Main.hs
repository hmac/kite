{-# LANGUAGE DeriveGeneric #-}
module Main where

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

import           Typecheck                      ( inferModule )

data Config =
      Repl
    | Run FilePath
    | Typecheck FilePath
    | Parse FilePath
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
    DumpLc      f -> dumpLC f
    DumpTypeEnv f -> dumpTypeEnv f
    Typecheck   f -> typecheck f

parse :: FilePath -> IO ()
parse = withParsedFile pPrint

dumpLC :: FilePath -> IO ()
dumpLC = withParsedFile (pPrint . ModuleGroupCompiler.compileModule)

dumpTypeEnv :: FilePath -> IO ()
dumpTypeEnv = withParsedFile (pPrint . ModuleGroupTypechecker.dumpEnv)

typecheck :: FilePath -> IO ()
typecheck = withParsedFile $ \(ModuleGroup m deps) ->
  let modules = deps ++ [m]
  in  case mapM_ inferModule modules of
        Left  err -> print err
        Right _   -> putStrLn "Success."

run :: FilePath -> IO ()
run = withParsedFile $ \g -> case ModuleGroupTypechecker.typecheckModule g of
  Left err -> putStrLn (printError err)
  Right _ ->
    let cm     = ModuleGroupCompiler.compileModule g
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
