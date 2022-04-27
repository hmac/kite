module Util
  ( module Util
  , Text.Pretty.Simple.pPrint
  , Data.Maybe.mapMaybe
  , Data.Maybe.fromMaybe
  , Data.Maybe.catMaybes
  , Data.Maybe.isJust
  , Data.Maybe.fromJust
  , Data.List.nub
  , Data.List.sort
  , Data.List.sortOn
  , Data.List.find
  , Data.List.partition
  , Data.List.unzip3
  , Data.List.unzip4
  , Data.List.unzip5
  , Data.List.foldl'
  , Data.List.Extra.nubOn
  , Data.List.Extra.nubOrdOn
  , Data.List.Extra.snoc
  , Data.List.NonEmpty.NonEmpty(..)
  , Data.Either.lefts
  , Data.Either.rights
  , Control.Monad.forM
  , Control.Monad.forM_
  , Control.Monad.foldM
  , Control.Monad.replicateM
  , Control.Monad.zipWithM
  , Control.Monad.Extra.mconcatMapM
  , Debug.Trace.trace
  , Debug.Trace.traceShow
  , Debug.Trace.traceShowId
  ) where

import qualified Control.Monad
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , liftEither
                                                , runExceptT
                                                )
import qualified Control.Monad.Extra
import qualified Data.Bifunctor
import qualified Data.Bitraversable
import qualified Data.Either
import qualified Data.List
import qualified Data.List.Extra
import           Data.List.Extra                ( concatUnzip
                                                , concatUnzip3
                                                )
import qualified Data.List.NonEmpty
import qualified Data.Maybe
import           Data.Text.Lazy                 ( unpack )
import qualified Debug.Trace
import qualified Text.Pretty.Simple
-- Misc useful functions

first :: Data.Bifunctor.Bifunctor p => (a -> b) -> p a c -> p b c
first = Data.Bifunctor.first

firstM :: Functor m => (a -> m a') -> (a, b) -> m (a', b)
firstM f (a, b) = (, b) <$> f a

second :: Data.Bifunctor.Bifunctor p => (c -> d) -> p a c -> p a d
second = Data.Bifunctor.second

secondM :: Functor m => (c -> m d) -> (a, c) -> m (a, d)
secondM f (a, b) = (a, ) <$> f b

bimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
bimap = Data.Bifunctor.bimap

bimapM
  :: (Data.Bitraversable.Bitraversable t, Applicative f)
  => (a -> f b)
  -> (c -> f d)
  -> t a c
  -> f (t b d)
bimapM = Data.Bitraversable.bimapM

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

-- | Uses a function to determine which of two output lists an input element should join
-- Taken from GHC.Util
partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith _ []       = ([], [])
partitionWith f (x : xs) = case f x of
  Left  b -> (b : bs, cs)
  Right c -> (bs, c : cs)
  where (bs, cs) = partitionWith f xs

-- Pretty printing
pShow :: Show a => a -> String
pShow = unpack . Text.Pretty.Simple.pShow

pTrace :: Show a => a -> b -> b
pTrace x = Debug.Trace.trace (pShow x)

pTraceId :: Show a => a -> a
pTraceId x = pTrace x x

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = Data.List.nub xs /= xs

-- | Monadic version of mapAccumL (from ghc)
mapAccumLM
  :: Monad m
  => (acc -> x -> m (acc, y)) -- ^ combining function
  -> acc                      -- ^ initial state
  -> [x]                      -- ^ inputs
  -> m (acc, [y])             -- ^ final state, outputs
mapAccumLM _ s []       = return (s, [])
mapAccumLM f s (x : xs) = do
  (s1, x' ) <- f s x
  (s2, xs') <- mapAccumLM f s1 xs
  return (s2, x' : xs')

deleteList :: Ord k => [k] -> [(k, v)] -> [(k, v)]
deleteList ks = Data.List.filter (not . (`elem` ks) . fst)

-- A typeclass for nice debugging output
-- We use this to compactly print expressions in typechecking traces, for example
class Debug a where
  debug :: a -> String

instance Debug Char where
  debug c = [c]

instance Debug a => Debug [a] where
  debug = concatMap debug

instance Debug a => Debug (Data.List.NonEmpty.NonEmpty a) where
  debug = debug . Data.List.NonEmpty.toList

-- | Convert a 'MonadError' constraint with a smaller error to one with a larger one.
wrapError :: (MonadError e m) => (e' -> e) -> ExceptT e' m a -> m a
wrapError f m = runExceptT m >>= (liftEither . first f)
