module Main where

import           Data.List.Extra                ( stripSuffix )
import           Data.Maybe                     ( mapMaybe )
import           Data.String                    ( IsString(fromString) )

import           Criterion.Main
import           System.FilePath.Posix          ( takeFileName )
import           System.Directory.Extra         ( listFilesRecursive )

import           Syn.Parse                      ( parseLamFile )
import           Syn
import           Canonicalise                   ( canonicaliseModule )

import           ModuleGroup                    ( ModuleGroup(..)
                                                , TypedModuleGroup(..)
                                                )
import           ModuleLoader                   ( loadFromPathAndRootDirectory )
import           ModuleGroupTypechecker         ( typecheckModuleGroup )
import           ModuleGroupCompiler            ( compileToLC
                                                , CompiledModule(..)
                                                )
import           LC.Eval                        ( evalMain )
import qualified LC

import           Constraint.Generate.M          ( run )
import           Constraint.Generate.Module     ( generateModule )
import qualified Constraint.Primitive

import           Util

-- We benchmark parsing and typechecking performance by parsing and typechecking
-- the Data.List module from the standard library. Note that the typechecking
-- time includes time spent parsing, ordering and typechecking dependencies.

main :: IO ()
main = defaultMain
  [ bgroup
    "parse"
    [bench "Data.List" $ nfIO $ parseFromPath "std/Data/List.lam" "Data.List"]
  , bgroup
    "typecheck"
    [ bench "Data.List" $ nfIO $ typecheckFromPathAndRoot "std/Data/List.lam"
                                                          "std"
    , bench "Data.List.Intersperse" $ nfIO $ typecheckModule exampleModule
    ]
  , bgroup
    "eval"
    [bench "Data.ListTest" $ nfIO $ runFromPathAndRoot "std/ListTest.lam" "std"]
  ]

runFromPathAndRoot path root = do
  group <- loadFromPathAndRootDirectory path root
  case group of
    Left  err -> error $ path <> ":\n" <> err
    Right g   -> case typecheckModuleGroup g of
      Left err -> error $ path <> ":\n" <> show err
      Right typedGroup ->
        let compiled = compileToLC typedGroup
        in  case evalMain (cModuleName compiled) (cModuleEnv compiled) of
              LC.Var "notARealVariable" -> pure True
              _                         -> pure False

typecheckFromPathAndRoot :: String -> String -> IO Bool
typecheckFromPathAndRoot path root = do
  group <- loadFromPathAndRootDirectory path root
  case group of
    Left  err -> error $ path <> ":\n" <> err
    Right g   -> case typecheckModuleGroup g of
      Left  err                       -> error $ path <> ":\n" <> show err
      Right (TypedModuleGroup m deps) -> pure True

typecheckModule :: Module -> IO Bool
typecheckModule m =
  let (res, _) =
          run $ generateModule Constraint.Primitive.env (canonicaliseModule m)
  in  case res of
        Left err -> error $ show (moduleName m) <> ":\n" <> show err
        Right (_env, typedModule) -> pure True

parseFromPath :: String -> ModuleName -> IO Bool
parseFromPath path modName = do
  contents <- readFile path
  case parseLamFile contents of
    Left  err -> error $ path <> ": expected parse success but failed\n" <> err
    Right m   -> pure $ moduleName m == modName

exampleModule :: Module
exampleModule = Module
  { moduleName     = "Data.List.Intersperse"
  , moduleImports  = []
  , moduleExports  = []
  , moduleMetadata = []
  , moduleDecls    =
    [ FunDecl
      (Fun
        { funComments = []
        , funName     = "intersperse"
        , funType     =
          Just
          $       TyVar "a"
          `fn`    TyList
          `tyapp` TyVar "a"
          `fn`    TyList
          `tyapp` TyVar "a"
        , funDefs     =
          [ Def { defArgs = [WildPat, ListPat []], defExpr = ListLit [] }
          , Def
            { defArgs = [VarPat "e", ConsPat "::" [VarPat "x", VarPat "xs"]]
            , defExpr = App
                          (App (Con "::") (Var "x"))
                          (App (App (Var "intersperseHelper") (Var "e"))
                               (Var "xs")
                          )
            }
          ]
        }
      )
    , FunDecl
      (Fun
        { funComments = []
        , funName     = "intersperseHelper"
        , funType     =
          Just
          $       TyVar "a"
          `fn`    TyList
          `tyapp` TyVar "a"
          `fn`    TyList
          `tyapp` TyVar "a"
        , funDefs     = [ Def { defArgs = [WildPat, ListPat []]
                              , defExpr = ListLit []
                              }
                        , Def
                          { defArgs = [ VarPat "e"
                                      , ConsPat "::" [VarPat "x", VarPat "xs"]
                                      ]
                          , defExpr = App
                            (App (Con "::") (Var "e"))
                            (App
                              (App (Con "::") (Var "x"))
                              (App (App (Var "intersperseHelper") (Var "e"))
                                   (Var "xs")
                              )
                            )
                          }
                        ]
        }
      )
    ]
  }
