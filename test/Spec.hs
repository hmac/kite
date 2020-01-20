import           Test.Hspec
import qualified ParseTest
import qualified PrintTest
import qualified RoundTrip
import qualified ConstraintTest
import qualified Test.Constraint.Pattern

main :: IO ()
main = hspec $ do
  ParseTest.test
  PrintTest.test
  RoundTrip.test
  ConstraintTest.test
  Test.Constraint.Pattern.test
