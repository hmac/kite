module Util
  ( module Util
  , Text.Pretty.Simple.pPrint
  )
where

import           Data.Text.Lazy                 ( unpack )
import qualified Text.Pretty.Simple
import qualified Data.Bifunctor
import           Data.List.Extra                ( concatUnzip
                                                , concatUnzip3
                                                )
-- Misc useful functions

first :: (a -> b) -> (a, c) -> (b, c)
first = Data.Bifunctor.first

second :: (c -> d) -> (a, c) -> (a, d)
second = Data.Bifunctor.second

bimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
bimap = Data.Bifunctor.bimap

mapFst :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFst f = map (Data.Bifunctor.first f)

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f = map (Data.Bifunctor.second f)

concat2 :: [([a], [b])] -> ([a], [b])
concat2 = concatUnzip

concat3 :: [([a], [b], [c])] -> ([a], [b], [c])
concat3 = concatUnzip3

concat4 :: [([a], [b], [c], [d])] -> ([a], [b], [c], [d])
concat4 [] = ([], [], [], [])
concat4 ((as, bs, cs, ds) : xs) =
  let (as', bs', cs', ds') = concat4 xs
  in  (as <> as', bs <> bs', cs <> cs', ds <> ds')

-- | Split a list into sublists delimited by the given element.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
 where
  go []       acc = [reverse acc]
  go (y : ys) acc = if x == y then reverse acc : go ys [] else go ys (y : acc)

-- Pretty printing
pShow :: Show a => a -> String
pShow = unpack . Text.Pretty.Simple.pShow
