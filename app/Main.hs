{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Syn.Print

import           Control.Monad                  ( void )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.UUID.V4                   ( nextRandom )
import           System.Directory               ( getCurrentDirectory )
import           System.Environment             ( lookupEnv )
import           System.Exit                    ( exitWith )
import           System.IO                      ( IOMode(WriteMode)
                                                , hPutStrLn
                                                , stdout
                                                , withFile
                                                )
import           System.Process.Typed           ( proc
                                                , runProcess
                                                )
import           Text.Pretty.Simple             ( pPrint )

import           ModuleGroup
import           ModuleGroupCompiler            ( CompiledModule(..) )
import qualified ModuleGroupCompiler
import qualified ModuleGroupTypechecker
import qualified ModuleLoader

import           Data.Name                      ( Name(TopLevel)
                                                , PackageName
                                                )
import           Options.Generic
import qualified Repl                           ( run )

import qualified Syn
import           Syn.Parse                      ( parseKiteFile )

import qualified Chez.Print
import           Interpret                      ( interpretAndRunMain
                                                , printValue
                                                )
import           Print                          ( Document
                                                , Style(..)
                                                )
import           Type.Print

data Config =
      Repl
    | Format FilePath
    | Eval FilePath
    | Run FilePath
    | Typecheck FilePath
    | Dump DumpPhase FilePath
    | Compile FilePath FilePath
    deriving (Generic, Show)

instance ParseRecord Config

data DumpPhase =
    AfterParse
  | BeforeTypecheck
  | AfterTypecheck
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
    Repl                   -> Repl.run
    Format    f            -> format f
    Eval      f            -> eval homeDir f
    Run       f            -> run homeDir f
    Typecheck f            -> typecheck homeDir f
    Compile inFile outFile -> compile homeDir inFile outFile
    Dump    phase  f       -> case phase of
      AfterParse      -> parse homeDir f
      BeforeTypecheck -> dumpTypeEnv homeDir f
      AfterTypecheck  -> dumpTypeEnv homeDir f -- TODO: currently this is before typechecking
      Chez            -> dumpChez homeDir f

parse :: FilePath -> FilePath -> IO ()
parse homeDir = withParsedFile "fake-pkg" homeDir pPrint

dumpChez :: FilePath -> FilePath -> IO ()
dumpChez homeDir = withParsedFile "fake-pkg" homeDir $ \g ->
  case ModuleGroupTypechecker.typecheckModuleGroup g of
    Left err -> printNicely (printLocatedError err)
    Right g' ->
      let cg      = ModuleGroupCompiler.compileToChez g'
          defs    = cModuleEnv cg
          program = Chez.Print.printProgram defs
      in  printNicely program

dumpTypeEnv :: FilePath -> FilePath -> IO ()
dumpTypeEnv homeDir = withParsedFile "fake-pkg" homeDir $ \g ->
  case ModuleGroupTypechecker.dumpEnv g of
    Left  err -> pPrint err
    Right g'  -> pPrint g'

typecheck :: FilePath -> FilePath -> IO ()
typecheck homeDir = withParsedFile "fake-pkg" homeDir $ \g ->
  case ModuleGroupTypechecker.typecheckModuleGroup g of
    Left  err -> printNicely $ printLocatedError err
    Right _   -> printNicely "Success."

format :: FilePath -> IO ()
format path = (parseKiteFile path "fake-pkg" <$> readFile path) >>= \case
  Right m   -> printNicely (printModule m)
  Left  err -> putStrLn err

eval :: FilePath -> FilePath -> IO ()
eval homeDir = withParsedFile "fake-pkg" homeDir $ \g ->
  case ModuleGroupTypechecker.typecheckModuleGroup g of
    Left err -> print (printLocatedError err)
    Right g' ->
      let answer = interpretAndRunMain g' in printNicely (printValue answer)

run :: FilePath -> FilePath -> IO ()
run homeDir inFile = flip (withParsedFile "fake-pkg" homeDir) inFile $ \g -> do
  -- TODO: are we typechecking here? we should be!
  let modName  = let ModuleGroup m _ = g in Syn.moduleName m
  let mainName = TopLevel modName "main"
  uuid1 <- nextRandom
  -- Compile the program to /tmp/<module name>-<random uuid>.ss
  let outFile = "/tmp/" <> show modName <> "-" <> show uuid1 <> ".ss"
  compileSuccess <- compileModuleGroup outFile g

  if compileSuccess
    then do
    -- Write a scheme script to load and run the program
      uuid2 <- nextRandom
      let scriptFile = "/tmp/" <> show modName <> "-" <> show uuid2 <> ".ss"
      withFile scriptFile WriteMode $ \h -> do
        hPutStrLn h $ "(load " <> show outFile <> ")"
        hPutStrLn h $ "(Kite.Primitive.runIO " <> show mainName <> ")"
      -- Run the script
      exitcode <- runProcess
        (proc "/usr/bin/env" ["scheme", "--script", scriptFile])
      exitWith exitcode
    else pure ()

compile :: FilePath -> FilePath -> FilePath -> IO ()
compile homeDir inFile outFile =
  withParsedFile "fake-pkg" homeDir (void . compileModuleGroup outFile) inFile

compileModuleGroup :: FilePath -> UntypedModuleGroup -> IO Bool
compileModuleGroup outFile g =
  case ModuleGroupTypechecker.typecheckModuleGroup g of
    Left err -> printNicely (printLocatedError err) >> pure False
    Right g' ->
      let cg       = ModuleGroupCompiler.compileToChez g'
          defs     = cModuleEnv cg
          chezCode = layout $ Chez.Print.printProgram defs
      in  withFile outFile WriteMode $ \handle -> do
            renderIO handle chezCode
            hPutStrLn handle ""
            pure True

withParsedFile
  :: PackageName
  -> FilePath
  -> (UntypedModuleGroup -> IO ())
  -> FilePath
  -> IO ()
withParsedFile pkgName homeDir cb path = do
  mgroup <- ModuleLoader.loadFromPathAndRootDirectory path homeDir pkgName
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
