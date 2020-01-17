module Util
  ( module Util
  , Text.Pretty.Simple.pPrint
  , Data.Maybe.mapMaybe
  , Data.Maybe.fromMaybe
  , Data.List.nub
  , Data.List.sort
  , Data.List.partition
  , Data.List.Extra.nubOn
  , Control.Monad.forM
  )
where

import qualified Control.Monad
import qualified Data.List.Extra
import qualified Data.List
import qualified Data.Maybe
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

bimapL :: (a -> b) -> (c -> d) -> [(a, c)] -> [(b, d)]
bimapL f g = map (bimap f g)

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

concat5 :: [([a], [b], [c], [d], [e])] -> ([a], [b], [c], [d], [e])
concat5 [] = ([], [], [], [], [])
concat5 ((as, bs, cs, ds, es) : xs) =
  let (as', bs', cs', ds', es') = concat5 xs
  in  (as <> as', bs <> bs', cs <> cs', ds <> ds', es <> es')

concat6 :: [([a], [b], [c], [d], [e], [f])] -> ([a], [b], [c], [d], [e], [f])
concat6 [] = ([], [], [], [], [], [])
concat6 ((as, bs, cs, ds, es, fs) : xs) =
  let (as', bs', cs', ds', es', fs') = concat6 xs
  in  (as <> as', bs <> bs', cs <> cs', ds <> ds', es <> es', fs <> fs')

concat7
  :: [([a], [b], [c], [d], [e], [f], [g])]
  -> ([a], [b], [c], [d], [e], [f], [g])
concat7 [] = ([], [], [], [], [], [], [])
concat7 ((as, bs, cs, ds, es, fs, gs) : xs) =
  let (as', bs', cs', ds', es', fs', gs') = concat7 xs
  in  ( as <> as'
      , bs <> bs'
      , cs <> cs'
      , ds <> ds'
      , es <> es'
      , fs <> fs'
      , gs <> gs'
      )

-- | Split a list into sublists delimited by the given element.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
 where
  go []       acc = [reverse acc]
  go (y : ys) acc = if x == y then reverse acc : go ys [] else go ys (y : acc)

-- Pretty printing
pShow :: Show a => a -> String
pShow = unpack . Text.Pretty.Simple.pShow
