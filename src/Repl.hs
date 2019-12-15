module Repl where

import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Prettyprint.Doc
import           System.IO                      ( stdout
                                                , hSetBuffering
                                                , BufferMode(..)
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Syn.Parse                      ( pExpr
                                                , pDecl
                                                )
import           Text.Megaparsec                ( parse
                                                , errorBundlePretty
                                                , (<|>)
                                                )
import           Typecheck.THIH                 ( initialEnv
                                                , Error(..)
                                                , tiProgram
                                                )
import           Typecheck.Translate            ( primitiveInsts
                                                , tiModule
                                                , toProgram
                                                , funToImpl
                                                )
import           Typecheck.Desugar              ( desugarModule
                                                , desugarFun
                                                )

import           ELC.Compile                    ( translateModule
                                                , defaultEnv
                                                )
import           LC.Compile                     ( runConvert
                                                , convertEnv
                                                )
import qualified LC.Eval                        ( evalVar )
import qualified LC.Print                       ( print )
import           Syntax
import qualified Canonicalise                  as Can
import           Canonical                      ( Name(..) )

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  repl []

repl :: [Decl Syn] -> IO ()
repl env = do
  putStrLn "Welcome to the Lam REPL."
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
    (env, assumps, prog) = toProgram mempty modCore
    mainBindGroup        = ([], [[funToImpl env (desugarFun main)]])
    classEnv = fromMaybe (error "failed to construct prelude typeclasses")
                         (primitiveInsts initialEnv)
    prog' = prog ++ [mainBindGroup]
  in
    case tiProgram classEnv assumps prog' of
      Left  (Error err) -> putStrLn err
      Right _           -> do
        let
          m' = Can.canonicaliseModule m
            { moduleDecls = FunDecl main : moduleDecls m
            }
        let answer =
              LC.Eval.evalVar (TopLevel (moduleName m') "$main") $ runConvert
                (translateModule ELC.Compile.defaultEnv m' >>= convertEnv)
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
