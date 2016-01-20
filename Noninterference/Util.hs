{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Noninterference.Util where

import Algebra.Lattice
import Unicode

import Data.Set as S
import Data.Set.Unicode

import Prelude as P
import Control.Monad (filterM)


class Enumerable a where
  allValues :: [a]

instance (Bounded a, Enum a) => Enumerable a where
  allValues = [minBound..]


powerset :: Ord a => Set a -> Set (Set a)
powerset = S.fromList . fmap S.fromList . listPowerset . S.toList

listPowerset :: [a] -> [[a]]
listPowerset = filterM (const [True, False])



showMapFun :: (Show a, Show b, Enumerable a, Ord a, Ord b) => (a -> b) -> String
showMapFun f = show $ fromList $ [(x, f x) | x <- allValues]


intersections :: (Ord a, Enum a, Bounded a) => [Set a] -> Set a
intersections = P.foldl S.intersection (fromList allValues)

lowerAdjoint :: (Bounded d', Enum d', Ord d', Ord d) => (Set d' -> Set d) -> (Set d -> Set d')
lowerAdjoint g ds = intersections [ ds' | ds' <- toList $ powerset (fromList allValues), ds ⊆ g ds' ]

upperAdjoint :: (Bounded d, Enum d, Ord d', Ord d) => (Set d -> Set d') -> (Set d' -> Set d)
upperAdjoint f ds' = unions [ ds | ds <- toList $ powerset (fromList allValues), f ds ⊆ ds' ]

gFrom g0 ds' = unions [ g0 d' | d' <- toList ds']
