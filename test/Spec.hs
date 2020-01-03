import           Test.Hspec
import qualified ParseTest
import qualified PrintTest
import qualified RoundTrip
import qualified ConstraintTest

main :: IO ()
main = hspec $ do
  ParseTest.test
  PrintTest.test
  RoundTrip.test
  ConstraintTest.test
