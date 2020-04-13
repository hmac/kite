module Integration.Typecheck
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
import           ModuleGroupTypechecker

test :: Spec
test = describe "typechecking Lam modules" $ do
  describe "expected typecheck passes"
    $ testEachFile expectTypecheckPass "test/fixtures/typecheck/pass"
  describe "expected typecheck failures"
    $ testEachFile expectTypecheckFail "test/fixtures/typecheck/fail"
  describe "expected naming failures"
    $ testEachFile expectNamingFail "test/fixtures/naming/fail"

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

expectNamingFail :: FilePath -> Expectation
expectNamingFail path = do
  res <- parseFile path
  case res of
    Left (NameError _) -> pure ()
    Left e ->
      expectationFailure
        $  "expected name resolution failure but found error "
        <> show e
    Right _ ->
      expectationFailure "expected name resolution error but succeeded"

parseFile :: FilePath -> IO (Either Error UntypedModuleGroup)
parseFile path = do
  mgroup <- ModuleLoader.loadFromPathAndRootDirectory path (takeDirectory path)
  case mgroup of
    Left  e -> pure $ Left e
    Right g -> pure $ Right g
