module Test.Diff
  ( shouldBe
  ) where

import           Data.Algorithm.DiffContext     ( getContextDiff
                                                , prettyContextDiff
                                                )
import           Test.HUnit.Lang                ( assertEqual )
import qualified Text.PrettyPrint              as Doc
import           Text.Show.Pretty               ( ppShow )

-- | Like 'shouldBe' from HSpec but renders a diff of the two values.
-- https://github.com/UnkindPartition/tasty/issues/226#issuecomment-508868067
shouldBe :: (Eq a, Show a) => a -> a -> IO ()
shouldBe got expected = assertEqual (show diff) got expected
 where
  gotPurdy      = lines $ ppShow got
  expectedPurdy = lines $ ppShow expected
  diff          = prettyContextDiff "Got"
                                    "Expected"
                                    Doc.text
                                    (getContextDiff 5 gotPurdy expectedPurdy)
