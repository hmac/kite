module Repl where

import           Control.Monad.Except           ( runExceptT )
import           Data.Functor                   ( ($>) )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Syn.Parse                      ( pDecl
                                                , pExpr
                                                , parse
                                                , spaceConsumerN
                                                )
import           System.IO                      ( BufferMode(..)
                                                , hSetBuffering
                                                , stdout
                                                )
import           Text.Megaparsec                ( (<|>)
                                                , eof
                                                , try
                                                )
import           Text.Megaparsec.Char           ( string )


import qualified Canonical                     as Can
import           Canonicalise                   ( canonicaliseModule )
import           Data.Name
import           Interpret                      ( interpretAndRun
                                                , printValue
                                                )
import qualified ModuleGroupTypechecker
import           ModuleLoader                   ( ModuleGroup(..) )
import           Syn

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
  let main = FunDecl Fun { funComments = []
                         , funName     = "$main"
                         , funType     = Nothing
                         , funExpr     = e
                         , funWheres   = []
                         }
      g = ModuleGroup (buildModule (decls ++ [main])) []
  in  case ModuleGroupTypechecker.typecheckModuleGroup g of
        Left  err -> putStrLn $ "Type error: " <> show err
        Right g'  -> do
          let modul = let (ModuleGroup m _) = g in m
          answer <- runExceptT
            $ interpretAndRun (TopLevel (moduleName modul) "$main") g'
          renderIO stdout $ layoutSmart defaultLayoutOptions $ case answer of
            Left  err   -> pretty err
            Right value -> printValue value
          putStrLn ""

buildModule :: [Decl Syn] -> Can.Module
buildModule decls = canonicaliseModule Module { moduleName = "kite-repl.Repl"
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
              putStrLn e
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
