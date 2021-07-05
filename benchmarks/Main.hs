module Main where


import           Criterion.Main

import           AST
import           Canonicalise                   ( canonicaliseModule )
import           Control.Monad.Except           ( runExceptT )
import           Data.Name                      ( PkgModuleName(..) )
import           Syn
import           Syn.Parse                      ( parseKiteFile )

import           ModuleGroup                    ( TypedModuleGroup(..) )
import           ModuleGroupTypechecker         ( typecheckModuleGroup )
import           ModuleLoader                   ( loadFromPathAndRootDirectory )

import           Type                           ( defaultTypeEnv
                                                , runTypecheckM
                                                )
import           Type.Module                    ( checkModule )


-- We benchmark parsing and typechecking performance by parsing and typechecking
-- the Data.List module from the standard library. Note that the typechecking
-- time includes time spent parsing, ordering and typechecking dependencies.

main :: IO ()
main = defaultMain
  [ bgroup
    "parse"
    [ bench "Data.List" $ nfIO $ parseFromPath "pkg/std/Data/List.kite"
                                               "Data.List"
    ]
  , bgroup
    "typecheck"
    [ bench "Data.List" $ nfIO $ typecheckFromPathAndRoot
      "pkg/std/Data/List.kite"
      "pkg/std"
    , bench "Data.List.Intersperse" $ nfIO $ typecheckModule exampleModule
    ]
  , bgroup "eval" []
  ]


typecheckFromPathAndRoot :: String -> String -> IO Bool
typecheckFromPathAndRoot path root = do
  group <- runExceptT $ loadFromPathAndRootDirectory path root "kite-benchmarks"
  case group of
    Left  err -> error $ path <> ":\n" <> show err
    Right g   -> case typecheckModuleGroup g of
      Left  err                    -> error $ path <> ":\n" <> show err
      Right (TypedModuleGroup _ _) -> pure True

typecheckModule :: Module -> IO Bool
typecheckModule m =
  let res = runTypecheckM defaultTypeEnv
        $ checkModule mempty (canonicaliseModule m)
  in  case res of
        Left  err -> error $ show (moduleName m) <> ":\n" <> show err
        Right _   -> pure True

parseFromPath :: String -> ModuleName -> IO Bool
parseFromPath path modName = do
  contents <- readFile path
  case parseKiteFile path "kite-benchmarks" contents of
    Left err -> error $ path <> ": expected parse success but failed\n" <> err
    Right m ->
      pure $ let (PkgModuleName _ mName) = moduleName m in mName == modName

exampleModule :: Module
exampleModule = Module
  { moduleName     = "std.Data.List.Intersperse"
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
                          , ( [ VarPat "e"
                              , ConsPat "::" Nothing [VarPat "x", VarPat "xs"]
                              ]
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
                          , ( [ VarPat "e"
                              , ConsPat "::" Nothing [VarPat "x", VarPat "xs"]
                              ]
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
