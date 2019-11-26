import           Test.Hspec
import qualified ParseTest
import qualified PrintTest

main :: IO ()
main = hspec $ do
  ParseTest.test
  PrintTest.test
