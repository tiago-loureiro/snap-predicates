module Data.Predicate.Delta
  ( Delta
  , empty
  , singleton
  , fromList
  , cons
  , size
  , append
  , (+++)
  , adjust
  , normalise
  , foldl
  , foldr
  , map
  , sum
  )
where

import Prelude hiding (foldl, foldr, sum, map)
import qualified Prelude as P
import qualified Data.List as L

-- | 'Delta' is a measure of distance. It is (optionally)
-- used in predicates that evaluate to 'T' but not uniquely so, i.e.
-- different evaluations of 'T' are possible and they may have a different
-- \"fitness\". The range of each 'Double' value is [0.0, 1.0].
--
-- An example is content-negotiation. A HTTP request may specify
-- a preference list of various media-types. A predicate matching one
-- specific media-type evaluates to 'T', but other media-types may match
-- even better. To represent this ambivalence, the predicate will include
-- a delta value which can be used to decide which of the matching
-- predicates should be preferred.
newtype Delta = Delta { _delta :: [Double] }

instance Show Delta where
    show (Delta d) = show d

instance Eq Delta where
    a == b =
        let [a', b'] = normalise [a, b]
        in _delta a' == _delta b'

instance Ord Delta where
    compare a b =
        let [a', b'] = normalise [a, b]
        in compare (_delta a') (_delta b')

empty :: Delta
empty = Delta []

singleton :: Double -> Delta
singleton d = Delta [d]

fromList :: [Double] -> Delta
fromList = Delta

cons :: Double -> Delta -> Delta
cons x (Delta d) = Delta (x:d)

append, (+++) :: Delta -> Delta -> Delta
append (Delta a) (Delta b) = Delta (a ++ b)

size :: Delta -> Int
size = length . _delta

(+++) = append

-- | Adjust the number of 'Double's in 'Delta'. If @i@ > 'size', then
-- the 'Delta' will be extended with 'size' - @i 1.0's@.
adjust :: Int -> Delta -> Delta
adjust i (Delta ds) = Delta $! take i (ds ++ [1.0, 1.0 ..])

-- | Normalisation means that for a list of 'Delta' values,
-- each 'Delta' is extended to the largest 'size' of all 'Delta's in
-- the list, by appending @1.0@s.
normalise :: [Delta] -> [Delta]
normalise ds = let len = maximum (P.map size ds) in P.map (adjust len) ds

foldl :: (a -> Double -> a) -> a -> Delta -> a
foldl f z (Delta d) = L.foldl' f z d

foldr :: (Double -> a -> a) -> a -> Delta -> a
foldr f z (Delta d) = L.foldr f z d

map :: (Double -> Double) -> Delta -> Delta
map f = foldr (\a b -> cons (f a) b) empty

sum :: Delta -> Double
sum = foldl (+) 0
