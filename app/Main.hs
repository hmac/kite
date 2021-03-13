{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Syn.Print

import           Text.Pretty.Simple             ( pPrint )
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Prettyprint.Doc
import           System.IO                      ( stdout )
import           System.Environment             ( lookupEnv )
import           System.Directory               ( getCurrentDirectory )
import           Control.Monad                  ( void )

import           ModuleGroup
import           ModuleGroupCompiler            ( CompiledModule(..) )
import qualified ModuleLoader
import qualified ModuleGroupTypechecker
import qualified ModuleGroupCompiler

import qualified Repl                           ( run )
import           LC.Execute                     ( executeMain )
import           Options.Generic

import           Syn.Parse                      ( parseKiteFile )

import           Print                          ( Document
                                                , Style(..)
                                                )
import           Type.Print
import           Interpret                      ( interpretAndRunMain
                                                , printValue
                                                )
import qualified Chez.Print

data Config =
      Repl
    | Format FilePath
    | Eval FilePath
    | Run FilePath
    | Typecheck FilePath
    | Dump DumpPhase FilePath
    deriving (Generic, Show)

instance ParseRecord Config

data DumpPhase =
    AfterParse
  | BeforeTypecheck
  | AfterTypecheck
  | LC
  | ELC
  | Chez
  deriving (Read, Eq, Generic, Show)

instance ParseField DumpPhase
instance ParseRecord DumpPhase
instance ParseFields DumpPhase

-- Parse stdin as a Kite module and pretty print the result
main :: IO ()
main = do
  homeDir <- lookupEnv "KITE_HOME" >>= \case
    Nothing -> getCurrentDirectory
    Just d  -> pure d
  cfg <- getRecord "kite"
  case cfg of
    Repl         -> Repl.run
    Format    f  -> format f
    Eval      f  -> eval homeDir f
    Run       f  -> run homeDir f
    Typecheck f  -> typecheck homeDir f
    Dump phase f -> case phase of
      AfterParse      -> parse homeDir f
      BeforeTypecheck -> dumpTypeEnv homeDir f
      AfterTypecheck  -> dumpTypeEnv homeDir f -- TODO: currently this is before typechecking
      LC              -> dumpLC homeDir f
      ELC             -> dumpELC homeDir f
      Chez            -> dumpChez homeDir f

parse :: FilePath -> FilePath -> IO ()
parse homeDir = withParsedFile homeDir pPrint

dumpLC :: FilePath -> FilePath -> IO ()
dumpLC homeDir = withParsedFile homeDir $ \g ->
  case ModuleGroupTypechecker.typecheckModuleGroup g of
    Left  err -> printNicely (printLocatedError err)
    Right g'  -> pPrint (ModuleGroupCompiler.compileToLC g')

dumpELC :: FilePath -> FilePath -> IO ()
dumpELC homeDir = withParsedFile homeDir $ \g ->
  case ModuleGroupTypechecker.typecheckModuleGroup g of
    Left  err -> printNicely (printLocatedError err)
    Right g'  -> pPrint (ModuleGroupCompiler.compileToELC g')

dumpChez :: FilePath -> FilePath -> IO ()
dumpChez homeDir = withParsedFile homeDir $ \g ->
  case ModuleGroupTypechecker.typecheckModuleGroup g of
    Left err -> printNicely (printLocatedError err)
    Right g' ->
      let cg   = ModuleGroupCompiler.compileToChez g'
          defs = cModuleEnv cg
      in  mapM_ (printNicely . Chez.Print.printDef) defs

dumpTypeEnv :: FilePath -> FilePath -> IO ()
dumpTypeEnv homeDir = withParsedFile homeDir $ \g ->
  case ModuleGroupTypechecker.dumpEnv g of
    Left  err -> pPrint err
    Right g'  -> pPrint g'

typecheck :: FilePath -> FilePath -> IO ()
typecheck homeDir = withParsedFile homeDir $ \g ->
  case ModuleGroupTypechecker.typecheckModuleGroup g of
    Left  err -> printNicely $ printLocatedError err
    Right _   -> printNicely "Success."

format :: FilePath -> IO ()
format fp = (parseKiteFile <$> readFile fp) >>= \case
  Right m   -> printNicely (printModule m)
  Left  err -> putStrLn err

eval :: FilePath -> FilePath -> IO ()
eval homeDir = withParsedFile homeDir $ \g ->
  case ModuleGroupTypechecker.typecheckModuleGroup g of
    Left err -> print (printLocatedError err)
    Right g' ->
      let answer = interpretAndRunMain g' in printNicely (printValue answer)

run :: FilePath -> FilePath -> IO ()
run homeDir = withParsedFile homeDir $ \g ->
  case ModuleGroupTypechecker.typecheckModuleGroup g of
    Left err -> print (printLocatedError err)
    Right g' ->
      let cm = ModuleGroupCompiler.compileToLC g'
      in  void $ executeMain (cModuleName cm) (cModuleEnv cm)

withParsedFile :: FilePath -> (UntypedModuleGroup -> IO ()) -> FilePath -> IO ()
withParsedFile homeDir cb path = do
  mgroup <- ModuleLoader.loadFromPathAndRootDirectory path homeDir
  case mgroup of
    Left  e -> putStrLn e
    Right g -> cb g

layout :: Document -> SimpleDocStream AnsiStyle
layout doc = reAnnotateS styleToColor (layoutSmart defaultLayoutOptions doc)

printNicely :: Document -> IO ()
printNicely doc = renderIO stdout (layout doc) >> putStrLn ""

-- Conor Colours
-- https://github.com/idris-lang/Idris-dev/blob/master/docs/reference/semantic-highlighting.rst
styleToColor :: Style -> AnsiStyle
styleToColor VarStyle      = color Magenta
styleToColor KeywordStyle  = bold
styleToColor FunctionStyle = color Green
styleToColor TypeStyle     = color Blue
styleToColor DataStyle     = color Red
styleToColor HoleStyle     = color Magenta <> italicized
