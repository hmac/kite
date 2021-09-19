{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Syn.Print

import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , runExceptT
                                                , throwError
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Text.Prettyprint.Doc
                                         hiding ( group )
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.UUID.V4                   ( nextRandom )
import           System.Exit                    ( exitWith )
import           System.IO                      ( IOMode(WriteMode)
                                                , hPutStrLn
                                                , stdout
                                                , withFile
                                                )
import           System.Process.Typed           ( proc
                                                , runProcess
                                                )
import           Util

import           ModuleGroup
import           ModuleGroupCompiler            ( CompiledModule(..) )
import qualified ModuleGroupCompiler
import qualified ModuleGroupTypechecker
import qualified ModuleLoader

import           Data.Name                      ( Name(TopLevel) )
import           Options.Generic
import qualified Repl                           ( run )

import qualified Syn
import           Syn.Parse                      ( parseKiteFile )

import qualified Chez.Print
import           Error                          ( Error(..) )
import           Interpret                      ( interpretAndRunMain
                                                , printValue
                                                )
import qualified Package
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
  cfg <- getRecord "kite"
  runApp $ case cfg of
    Repl                   -> liftIO Repl.run
    Format    f            -> format f
    Eval      f            -> eval f
    Run       f            -> run f
    Typecheck f            -> typecheck f
    Compile inFile outFile -> compile inFile outFile
    Dump    phase  f       -> case phase of
      AfterParse      -> parse f
      BeforeTypecheck -> dumpTypeEnv f
      AfterTypecheck  -> dumpTypeEnv f -- TODO: currently this is before typechecking
      Chez            -> dumpChez f

runApp :: ExceptT Error IO a -> IO ()
runApp m = runExceptT m >>= \case
  Left  err -> printNicely $ pretty err
  Right _   -> pure ()

parse :: (MonadIO m, MonadError Error m) => FilePath -> m ()
parse path = loadFile path >>= pPrint

dumpChez :: (MonadIO m, MonadError Error m) => FilePath -> m ()
dumpChez path = do
  group <- loadFile path
  case ModuleGroupTypechecker.typecheckModuleGroup group of
    Left  err    -> liftIO $ printNicely (printLocatedError err)
    Right group' -> do
      cg <- wrapError CompileError $ ModuleGroupCompiler.compileToChez group'
      let defs    = cModuleEnv cg
          program = Chez.Print.printProgram defs
      liftIO $ printNicely program

dumpTypeEnv :: (MonadIO m, MonadError Error m) => FilePath -> m ()
dumpTypeEnv path = do
  group <- loadFile path
  case ModuleGroupTypechecker.dumpEnv group of
    Left  err -> pPrint err
    Right g'  -> pPrint g'

typecheck :: (MonadIO m, MonadError Error m) => FilePath -> m ()
typecheck path = do
  group <- loadFile path
  liftIO $ case ModuleGroupTypechecker.typecheckModuleGroup group of
    Left  err -> printNicely $ printLocatedError err
    Right _   -> printNicely "Success."

format :: (MonadIO m, MonadError Error m) => FilePath -> m ()
format path = do
  fileContents <- wrapError LoadError $ ModuleLoader.readFile' path
  liftIO $ case parseKiteFile path "fake-pkg" fileContents of
    Right m   -> printNicely (printModule m)
    Left  err -> putStrLn err

eval :: (MonadFix m, MonadIO m, MonadError Error m) => FilePath -> m ()
eval path = do
  group <- loadFile path
  case ModuleGroupTypechecker.typecheckModuleGroup group of
    Left  err -> liftIO $ print (printLocatedError err)
    Right g'  -> do
      answer <- wrapError InterpretError $ interpretAndRunMain g'
      liftIO $ printNicely (printValue answer)

run :: (MonadIO m, MonadError Error m) => FilePath -> m ()
run path = do
  g@(ModuleGroup m _) <- loadFile path
  let modName  = Syn.moduleName m
  let mainName = TopLevel modName "main"
  uuid1 <- liftIO nextRandom
  -- Compile the program to /tmp/<module name>-<random uuid>.ss
  let outFile = "/tmp/" <> show modName <> "-" <> show uuid1 <> ".ss"
  compileModuleGroup outFile g

  -- Write a scheme script to load and run the program
  uuid2 <- liftIO nextRandom
  let scriptFile = "/tmp/" <> show modName <> "-" <> show uuid2 <> ".ss"
  liftIO $ withFile scriptFile WriteMode $ \h -> do
    hPutStrLn h $ "(load " <> show outFile <> ")"
    hPutStrLn h $ "(kite.Kite.Prim.runIO " <> show mainName <> ")"
  -- Run the script
  exitcode <- runProcess
    (proc "/usr/bin/env" ["scheme", "--script", scriptFile])
  liftIO $ exitWith exitcode

compile :: (MonadIO m, MonadError Error m) => FilePath -> FilePath -> m ()
compile inFile outFile = do
  group <- loadFile inFile
  _     <- compileModuleGroup outFile group
  pure ()

compileModuleGroup
  :: (MonadIO m, MonadError Error m) => FilePath -> UntypedModuleGroup -> m ()
compileModuleGroup outFile g =
  case ModuleGroupTypechecker.typecheckModuleGroup g of
    Left  err -> throwError $ TypeError err
    Right g'  -> do
      cg <- wrapError CompileError $ ModuleGroupCompiler.compileToChez g'
      let defs     = cModuleEnv cg
          chezCode = layout $ Chez.Print.printProgram defs
      liftIO $ withFile outFile WriteMode $ \handle -> do
        renderIO handle chezCode
        hPutStrLn handle ""

loadFile :: (MonadError Error m, MonadIO m) => FilePath -> m UntypedModuleGroup
loadFile path = do
  pkgInfo <- wrapError PackageError Package.loadAndBuildPackageInfo
  wrapError LoadError $ ModuleLoader.loadFromPackageInfo pkgInfo path

layout :: Document -> SimpleDocStream AnsiStyle
layout doc = reAnnotateS styleToColor (layoutSmart defaultLayoutOptions doc)

printNicely :: Document -> IO ()
printNicely doc = renderIO stdout (layout doc) >> putStrLn ""

-- Conor Colours
-- https://github.com/idris-lang/Idris-dev/blob/master/docs/reference/semantic-highlighting.rst
styleToColor :: Style -> AnsiStyle
styleToColor VarStyle      = color Magenta
styleToColor KeywordStyle  = bold <> color Cyan
styleToColor FunctionStyle = color Green
styleToColor TypeStyle     = color Blue
styleToColor DataStyle     = color Red
styleToColor HoleStyle     = color Magenta <> italicized
