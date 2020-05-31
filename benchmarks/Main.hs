module Main where

import           Data.List.Extra                ( stripSuffix )
import           Data.Maybe                     ( mapMaybe )
import           Data.String                    ( IsString(fromString) )

import           Criterion.Main
import           System.FilePath.Posix          ( takeFileName )
import           System.Directory.Extra         ( listFilesRecursive )

import           Syn.Parse                      ( parseLamFile )
import           Syn                            ( Module_(moduleName) )
import           Data.Name                      ( ModuleName )

import           ModuleGroup                    ( TypedModuleGroup(..) )
import           ModuleLoader                   ( loadFromPathAndRootDirectory )
import           ModuleGroupTypechecker         ( typecheckModuleGroup )

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
    ]
  ]

parseAndTypecheckAllStdModules :: IO [Benchmark]
parseAndTypecheckAllStdModules = do
  mods <- mapMaybe (stripSuffix ".lam") <$> listFilesRecursive "std"
  pure $ map
    (\path ->
      let modName = fromString (takeFileName path)
      in  bench modName (nfIO (typecheckFromPathAndRoot (path <> ".lam") "std"))
    )
    mods


parseAllStdModules :: IO [Benchmark]
parseAllStdModules = do
  mods <- mapMaybe (stripSuffix ".lam") <$> listFilesRecursive "std"
  pure $ map
    (\path ->
      let modName = fromString (takeFileName path)
      in  bench (show modName) (nfIO (parseFromPath (path <> ".lam") modName))
    )
    mods

typecheckFromPathAndRoot :: String -> String -> IO Bool
typecheckFromPathAndRoot path root = do
  group <- loadFromPathAndRootDirectory path root
  case group of
    Left  err -> error $ path <> ":\n" <> err
    Right g   -> case typecheckModuleGroup g of
      Left  err                       -> error $ path <> ":\n" <> show err
      Right (TypedModuleGroup m deps) -> pure True

parseFromPath :: String -> ModuleName -> IO Bool
parseFromPath path modName = do
  contents <- readFile path
  case parseLamFile contents of
    Left  err -> error $ path <> ": expected parse success but failed\n" <> err
    Right m   -> pure $ moduleName m == modName
