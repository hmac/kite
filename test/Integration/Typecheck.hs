module Integration.Typecheck
  ( test
  ) where

import           Control.Monad.Except           ( runExceptT )
import           System.Directory               ( listDirectory )
import           System.FilePath.Posix          ( (</>)
                                                , takeDirectory
                                                )
import           Test.Hspec

import           ModuleGroup
import           ModuleGroupTypechecker
import           ModuleLoader

test :: Spec
test = describe "typechecking Kite modules" $ do
  describe "expected passes"
    $ testEachFile expectTypecheckPass "test/fixtures/typecheck/pass"
  describe "expected failures"
    $ testEachFile expectTypecheckFail "test/fixtures/typecheck/fail"

testEachFile :: (FilePath -> Expectation) -> FilePath -> Spec
testEachFile testFn dirPath = do
  files <- runIO $ listDirectory dirPath
  mapM_ (\path -> it path (testFn (dirPath </> path))) files

expectTypecheckPass :: FilePath -> Expectation
expectTypecheckPass path = do
  res <- parseFile path
  case res of
    Left  err -> expectationFailure (show err)
    Right g   -> case typecheckModuleGroup g of
      Left  err -> expectationFailure (show err)
      Right _   -> pure ()

expectTypecheckFail :: FilePath -> Expectation
expectTypecheckFail path = do
  res <- parseFile path
  case res of
    Left  err -> expectationFailure (show err)
    Right g   -> case typecheckModuleGroup g of
      Left  _ -> pure ()
      Right _ -> expectationFailure "expected type error but succeeded"

parseFile :: FilePath -> IO (Either ModuleLoader.Error UntypedModuleGroup)
parseFile path = runExceptT $ ModuleLoader.loadFromPathAndRootDirectory
  path
  (takeDirectory path)
  "kite-integration-tests"
