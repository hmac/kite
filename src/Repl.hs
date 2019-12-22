module Repl where

import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Prettyprint.Doc
import           System.IO                      ( stdout
                                                , hSetBuffering
                                                , BufferMode(..)
                                                )
import           Syn.Parse                      ( pExpr
                                                , pDecl
                                                )
import           Text.Megaparsec                ( parse
                                                , errorBundlePretty
                                                , (<|>)
                                                )

import qualified LC.Eval                        ( evalVar )
import qualified LC.Print                       ( print )
import           Syntax
import           Canonical                      ( Name(..) )
import qualified ModuleGraphTypechecker
import           ModuleGraphCompiler            ( CompiledModule(..) )
import qualified ModuleGraphCompiler
import           ModuleLoader                   ( LoadedModule(..) )

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to the Lam REPL."
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

processDecl :: [Decl Syn] -> IO Bool
processDecl decls =
  let l = LoadedModule (buildModule decls) []
  in  case ModuleGraphTypechecker.typecheckModule l of
        Left err -> do
          print err
          pure False
        Right _ -> do
          putStrLn "OK."
          pure True

processExpr :: [Decl Syn] -> Syn -> IO ()
processExpr decls e =
  let
    main = FunDecl Fun { funComments   = []
                       , funName       = "$main"
                       , funType       = TyHole "replExpression"
                       , funConstraint = Nothing
                       , funDefs       = [Def { defArgs = [], defExpr = e }]
                       }
    m = buildModule (decls ++ [main])
    l = LoadedModule m []
  in
    case ModuleGraphTypechecker.typecheckModule l of
      Left  err -> print err
      Right _   -> do
        let compiled = ModuleGraphCompiler.compileModule l
        let answer = LC.Eval.evalVar
              (TopLevel (cModuleName compiled) "$main")
              (cModuleEnv compiled)
        renderIO stdout
                 (layoutPretty defaultLayoutOptions (LC.Print.print answer))
        putStrLn ""

buildModule :: [Decl Syn] -> Module Syn
buildModule decls = Module { moduleName     = ModuleName ["Repl"]
                           , moduleImports  = []
                           , moduleExports  = []
                           , moduleDecls    = decls
                           , moduleMetadata = []
                           }

data Input = Expression Syn | Definition (Decl Syn)

parseInput :: IO Input
parseInput = go []
 where
  go inputSoFar = do
    let prompt = if null inputSoFar then "λ " else "> "
    putStr prompt
    input <- getLine
    -- If the input ends in a backslash, assume there's more to come
    if last input == '\\'
      then go (init input : inputSoFar)
      else do
        let allInput = unlines (reverse (input : inputSoFar))
        case
            parse (Definition <$> pDecl <|> Expression <$> pExpr) "" allInput
          of
            Left e -> do
              putStrLn (errorBundlePretty e)
              go []
            Right e -> pure e
