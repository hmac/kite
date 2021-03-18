module Integration.Parse
  ( test
  )
where

import           System.Directory               ( listDirectory )
import           System.FilePath.Posix          ( (</>)
                                                , takeDirectory
                                                )
import           Test.Hspec

import           ModuleGroup
import           ModuleLoader

test :: Spec
test = describe "parsing Kite modules" $ do
  describe "expected passes"
    $ testEachFile expectParsePass "test/fixtures/parse/pass"
  describe "expected failures"
    $ testEachFile expectParseFail "test/fixtures/parse/fail"

testEachFile :: (FilePath -> Expectation) -> FilePath -> Spec
testEachFile testFn dirPath = do
  files <- runIO $ listDirectory dirPath
  mapM_ (\path -> it path (testFn (dirPath </> path))) files

expectParsePass :: FilePath -> Expectation
expectParsePass path = do
  res <- parseFile path
  case res of
    Left  err -> expectationFailure err
    Right _   -> pure ()

expectParseFail :: FilePath -> Expectation
expectParseFail path = do
  res <- parseFile path
  case res of
    Left  _ -> pure ()
    Right _ -> expectationFailure "expected parse error but succeeded"

parseFile :: FilePath -> IO (Either String UntypedModuleGroup)
parseFile path = ModuleLoader.loadFromPathAndRootDirectory path (takeDirectory path)
