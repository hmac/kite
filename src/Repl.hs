module Repl where

import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Functor                   ( ($>) )
import           Data.Text.Prettyprint.Doc
import           System.IO                      ( stdout
                                                , hSetBuffering
                                                , BufferMode(..)
                                                )
import           Syn.Parse                      ( pExpr
                                                , pDecl
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
import           Data.Name
import qualified Canonical                     as Can
import           Canonicalise                   ( canonicaliseModule )
import qualified ModuleGroupTypechecker
import           ModuleGroupCompiler            ( CompiledModule(..) )
import qualified ModuleGroupCompiler
import           ModuleLoader                   ( ModuleGroup(..) )

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to the Kite REPL."
  repl []

repl :: [Decl Syn] -> IO ()
repl env = do
  input <- parseInput
  case input of
    Definition decl -> do
      let env' = decl : env
      ok <- processDecl env'
      if ok then repl env' else repl env
    Expression e -> do
      processExpr env e
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
  putStrLn ":help - show this help text"
  putStrLn ":h    - show this help text"
  putStrLn ":quit - exit the repl"
  putStrLn ":q    - exit the repl"


processDecl :: [Decl Syn] -> IO Bool
processDecl decls =
  let g = ModuleGroup (buildModule decls) []
  in  case ModuleGroupTypechecker.typecheckModuleGroup g of
        Left err -> do
          putStrLn $ "Type error: " <> show err
          pure False
        Right _ -> do
          putStrLn "OK."
          pure True

processExpr :: [Decl Syn] -> Syn -> IO ()
processExpr decls e =
  let
    main = FunDecl Fun { funComments = []
                       , funName     = "$main"
                       , funType     = Nothing
                       , funExpr     = e
                       }
    g = ModuleGroup (buildModule (decls ++ [main])) []
  in
    case ModuleGroupTypechecker.typecheckModuleGroup g of
      Left  err -> putStrLn $ "Type error: " <> show err
      Right g'  -> do
        let compiled = ModuleGroupCompiler.compileToLC g'
        let answer = LC.Eval.evalVar
              (TopLevel (cModuleName compiled) "$main")
              (cModuleEnv compiled)
        renderIO stdout
                 (layoutSmart defaultLayoutOptions (LC.Print.print answer))
        putStrLn ""

buildModule :: [Decl Syn] -> Can.Module
buildModule decls = canonicaliseModule Module
  { moduleName     = ModuleName ["Repl"]
  , moduleImports  = []
  , moduleExports  = []
  , moduleDecls    = decls
  , moduleMetadata = []
  }

data Input = Command Command | Expression Syn | Definition (Decl Syn)
  deriving (Eq, Show)

data Command = Help | Quit
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
  command =
    (string "help" $> Help)
      <|> (string "h" $> Help)
      <|> (string "quit" $> Quit)
      <|> (string "q" $> Quit)
