module Repl where

import           Text.Pretty.Simple             ( pPrint )
import           System.IO                      ( stdout )
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

import           ELC                            ( translateExpr
                                                , translateModule
                                                , primConstructors
                                                )
import           LC                             ( runConvert
                                                , convertEnv
                                                , convert
                                                )
import qualified EvalLC                         ( eval )
import           Data.Maybe                     ( fromMaybe )
import           System.IO                      ( hSetBuffering
                                                , BufferMode(..)
                                                )
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
  let m = Module { moduleName     = ModuleName ["Repl"]
                 , moduleImports  = []
                 , moduleExports  = []
                 , moduleDecls    = decls
                 , moduleMetadata = []
                 }
      core = desugarModule m
  in  case tiModule core of
        Left  (Error err) -> putStrLn err >> pure False
        Right _           -> do
          let env =
                runConvert (translateModule primConstructors m >>= convertEnv)
          pPrint env
          pure True

processExpr :: [Decl Syn] -> Syn -> IO ()
processExpr decls e =
  let main = Fun { funComments = []
                 , funName     = "main"
                 , funType     = TyHole "replExpression"
                 , funDefs     = [Def { defArgs = [], defExpr = e }]
                 }
      m = Module { moduleName     = ModuleName ["Repl"]
                 , moduleImports  = []
                 , moduleExports  = []
                 , moduleDecls    = decls
                 , moduleMetadata = []
                 }
      modCore              = desugarModule m
      (env, assumps, prog) = toProgram modCore
      mainBindGroup        = ([], [[funToImpl env (desugarFun main)]])
      classEnv = fromMaybe (error "failed to construct prelude typeclasses")
                           (primitiveInsts initialEnv)
      prog' = prog ++ [mainBindGroup]
  in  case tiProgram classEnv assumps prog' of
        Left (Error err) -> putStrLn err
        Right _ ->
          -- TODO: the better way to do this is to bind e as a declaration with
          -- name $main and then just eval (Var "$main")
          let answer = runConvert $ do
                elcEnv <- translateModule primConstructors m
                elce   <- translateExpr elcEnv e
                lcEnv  <- convertEnv elcEnv
                lce    <- convert elce
                pure $ EvalLC.eval lcEnv lce
          in  pPrint answer

data Input = Expression Syn | Definition (Decl Syn)

parseInput :: IO Input
parseInput = go []
 where
  go inputSoFar = do
    let prompt = if null inputSoFar then ">" else "|"
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
