module Main where


import           Criterion.Main

import           Syn.Parse                      ( parseKiteFile )
import           AST
import           Syn
import           Canonicalise                   ( canonicaliseModule )

import           ModuleGroup                    ( TypedModuleGroup(..) )
import           ModuleLoader                   ( loadFromPathAndRootDirectory )
import           ModuleGroupTypechecker         ( typecheckModuleGroup )
import           ModuleGroupCompiler            ( compileToLC
                                                , CompiledModule(..)
                                                )
import           LC.Eval                        ( evalMain )
import qualified LC

import           Type.Module                    ( checkModule )
import           Type                           ( runTypeM
                                                , defaultTypeEnv
                                                )


-- We benchmark parsing and typechecking performance by parsing and typechecking
-- the Data.List module from the standard library. Note that the typechecking
-- time includes time spent parsing, ordering and typechecking dependencies.

main :: IO ()
main = defaultMain
  [ bgroup
    "parse"
    [bench "Data.List" $ nfIO $ parseFromPath "std/Data/List.kite" "Data.List"]
  , bgroup
    "typecheck"
    [ bench "Data.List" $ nfIO $ typecheckFromPathAndRoot "std/Data/List.kite"
                                                          "std"
    , bench "Data.List.Intersperse" $ nfIO $ typecheckModule exampleModule
    ]
  , bgroup
    "eval"
    [ bench "Data.ListTest" $ nfIO $ runFromPathAndRoot "std/ListTest.kite"
                                                        "std"
    ]
  ]

runFromPathAndRoot :: FilePath -> FilePath -> IO Bool
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
      Left  err                    -> error $ path <> ":\n" <> show err
      Right (TypedModuleGroup _ _) -> pure True

typecheckModule :: Module -> IO Bool
typecheckModule m =
  let res = runTypeM defaultTypeEnv $ checkModule mempty (canonicaliseModule m)
  in  case res of
        Left  err -> error $ show (moduleName m) <> ":\n" <> show err
        Right _   -> pure True

parseFromPath :: String -> ModuleName -> IO Bool
parseFromPath path modName = do
  contents <- readFile path
  case parseKiteFile path contents of
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
        , funWheres   = []
        , funName     = "intersperse"
        , funType     =
          Just
          $       TyVar "a"
          `fn`    TyList
          `tyapp` TyVar "a"
          `fn`    TyList
          `tyapp` TyVar "a"
        , funExpr     = MCase
                          [ ([WildPat, ListPat []], ListLit [])
                          , ( [VarPat "e", ConsPat "::" Nothing [VarPat "x", VarPat "xs"]]
                            , App
                              (App (Con "::") (Var "x"))
                              (App (App (Var "intersperseHelper") (Var "e"))
                                   (Var "xs")
                              )
                            )
                          ]
        }
      )
    , FunDecl
      (Fun
        { funComments = []
        , funWheres   = []
        , funName     = "intersperseHelper"
        , funType     =
          Just
          $       TyVar "a"
          `fn`    TyList
          `tyapp` TyVar "a"
          `fn`    TyList
          `tyapp` TyVar "a"
        , funExpr     = MCase
                          [ ([WildPat, ListPat []], ListLit [])
                          , ( [VarPat "e", ConsPat "::" Nothing [VarPat "x", VarPat "xs"]]
                            , App
                              (App (Con "::") (Var "e"))
                              (App
                                (App (Con "::") (Var "x"))
                                (App (App (Var "intersperseHelper") (Var "e"))
                                     (Var "xs")
                                )
                              )
                            )
                          ]
        }
      )
    ]
  }
