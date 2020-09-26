module Repl where

import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Prettyprint.Doc
import           System.Directory               ( getCurrentDirectory )
import           System.Environment             ( lookupEnv )
import           System.IO                      ( stdout
                                                , hSetBuffering
                                                , BufferMode(..)
                                                )
import           Syn.Parse                      ( pExpr
                                                , pDecl
                                                , pImport
                                                , spaceConsumerN
                                                )
import           Text.Megaparsec                ( parse
                                                , errorBundlePretty
                                                , (<|>)
                                                , eof
                                                , try
                                                )
import           Text.Megaparsec.Char           ( string )

import qualified LC.Eval                        ( evalVar )
import qualified LC.Print                       ( print )
import           Syn
import qualified Syn.Print
import           Data.Name
import qualified ModuleGroupTypechecker
import           ModuleGroupCompiler            ( CompiledModule(..) )
import qualified ModuleGroupCompiler
import           ModuleLoader                   ( loadModule )
import           Util

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  homeDir <- lookupEnv "KITE_HOME" >>= \case
    Nothing -> getCurrentDirectory
    Just d  -> pure d
  putStrLn "Welcome to the Kite REPL."
  repl Env { root = homeDir, decls = mempty, imports = mempty }

data Env = Env { root :: FilePath
               , decls :: [Decl Syn]
               , imports :: [Import]
               }

repl :: Env -> IO ()
repl env = do
  input <- parseInput
  case input of
    Definition decl -> do
      let env' = env { decls = decl : decls env }
      ok <- processDecl env'
      if ok then repl env' else repl env
    Expression e -> do
      processExpr env e
      repl env
    DoImport i -> do
      env' <- processImport env i
      repl env'
    Command PrintModule -> do
      printModule env
      repl env
    Command Help -> do
      showHelp
      repl env
    Command Quit -> do
      pure ()

showHelp :: IO ()
showHelp = do
  putStrLn "Help:"
  putStrLn "Enter an expression to evaluate it"
  putStrLn "Enter a definition to add it to the local environment"
  putStrLn "Or enter a command:"
  putStrLn ":help  - show this help text"
  putStrLn ":h     - show this help text"
  putStrLn ":print - print the current module"
  putStrLn ":quit  - exit the repl"
  putStrLn ":q     - exit the repl"

printModule :: Env -> IO ()
printModule env = do
  let m = buildModule env
  Syn.Print.printNicely (Syn.Print.printModule m)

processDecl :: Env -> IO Bool
processDecl env = do
  mg <- loadModule (root env) (buildModule env)
  case mg of
    Left err -> do
      putStrLn $ "Error: " <> show err
      pure False
    Right g -> case ModuleGroupTypechecker.typecheckModuleGroup g of
      Left err -> do
        putStrLn $ "Type error: " <> show err
        pure False
      Right _ -> do
        putStrLn "OK."
        pure True

processExpr :: Env -> Syn -> IO ()
processExpr env e =
  let
    main = FunDecl Fun { funComments = []
                       , funName     = "$main"
                       , funType     = Nothing
                       , funExpr     = e
                       }
    modul = buildModule env { decls = (decls env ++ [main]) }
  in
    do
      mg <- loadModule (root env) modul
      case pTrace mg mg of
        Left  err -> putStrLn $ "Parse error: " <> show err
        Right g   -> case ModuleGroupTypechecker.typecheckModuleGroup g of
          Left  err -> putStrLn $ "Type error: " <> show err
          Right g'  -> do
            let compiled = ModuleGroupCompiler.compileToLC g'
            let answer = LC.Eval.evalVar
                  (TopLevel (cModuleName compiled) "$main")
                  (cModuleEnv compiled)
            renderIO
              stdout
              (layoutSmart defaultLayoutOptions (LC.Print.print answer))
            putStrLn ""

processImport :: Env -> Import -> IO Env
processImport env imp = pure env { imports = imp : imports env }

buildModule :: Env -> Module
buildModule env = Module { moduleName     = ModuleName ["Repl"]
                         , moduleImports  = imports env
                         , moduleExports  = []
                         , moduleDecls    = decls env
                         , moduleMetadata = []
                         }

data Input = Command Command
           | Expression Syn
           | Definition (Decl Syn)
           | DoImport Import
  deriving (Eq, Show)

data Command = Help | Quit | PrintModule
  deriving (Eq, Show)

parseInput :: IO Input
parseInput = go []
 where
  go inputSoFar = do
    let prompt = if null inputSoFar then "λ " else "> "
    putStr prompt
    -- If the input ends in a backslash, assume there's more to come
    getLine >>= \case
      input
        | null input -> go inputSoFar
        | last input == '\\' -> go (init input : inputSoFar)
        | otherwise -> do
          let allInput = unlines (reverse (input : inputSoFar))
          case parse (parser <* spaceConsumerN <* eof) "" allInput of
            Left e -> do
              putStrLn (errorBundlePretty e)
              go []
            Right e -> pure e
  parser =
    Command
      <$> (string ":" *> command)
      <|> try (Definition <$> pDecl)
      <|> Expression
      <$> pExpr
      <|> DoImport
      <$> pImport
  command =
    (string "help" *> pure Help)
      <|> (string "h" *> pure Help)
      <|> (string "quit" *> pure Quit)
      <|> (string "q" *> pure Quit)
      <|> (string "print" *> pure PrintModule)
