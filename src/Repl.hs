module Repl where

import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Prettyprint.Doc
import           System.IO                      ( stdout
                                                , hSetBuffering
                                                , BufferMode(..)
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Parse                          ( pExpr
                                                , pDecl
                                                )
import           Text.Megaparsec                ( parse
                                                , errorBundlePretty
                                                , (<|>)
                                                )
import           THIH                           ( initialEnv
                                                , Error(..)
                                                , tiProgram
                                                )
import           Translate                      ( primitiveInsts
                                                , tiModule
                                                , toProgram
                                                , funToImpl
                                                )
import           Desugar                        ( desugarModule
                                                , desugarFun
                                                )

import           ELC                            ( translateModule
                                                , primConstructors
                                                )
import           LC                             ( runConvert
                                                , convertEnv
                                                )
import qualified LC.Eval                        ( evalVar )
import qualified LC.Print                       ( print )
import           Syntax

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
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
  let core = desugarModule (buildModule decls)
  in  case tiModule core of
        Left (Error err) -> do
          putStrLn err
          pure False
        Right _ -> do
          putStrLn "OK."
          pure True

processExpr :: [Decl Syn] -> Syn -> IO ()
processExpr decls e =
  let
    main = Fun { funComments = []
               , funName     = "$main"
               , funType     = TyHole "replExpression"
               , funDefs     = [Def { defArgs = [], defExpr = e }]
               }
    m                    = buildModule decls
    modCore              = desugarModule m
    (env, assumps, prog) = toProgram modCore
    mainBindGroup        = ([], [[funToImpl env (desugarFun main)]])
    classEnv = fromMaybe (error "failed to construct prelude typeclasses")
                         (primitiveInsts initialEnv)
    prog' = prog ++ [mainBindGroup]
  in
    case tiProgram classEnv assumps prog' of
      Left  (Error err) -> putStrLn err
      Right _           -> do
        let m' = m { moduleDecls = FunDecl main : moduleDecls m }
        let answer = LC.Eval.evalVar "$main" $ runConvert
              (translateModule primConstructors m' >>= convertEnv)
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
    if null input
      then do
        let allInput = unlines (reverse (input : inputSoFar))
        case
            parse (Definition <$> pDecl <|> Expression <$> pExpr) "" allInput
          of
            Left e -> do
              putStrLn (errorBundlePretty e)
              go []
            Right e -> pure e
      else go (input : inputSoFar)
