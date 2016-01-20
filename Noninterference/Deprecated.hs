-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}


module Noninterference.Deprecated where

import Noninterference

import Algebra.Lattice
import Unicode

import Data.Set as S
import Data.Set.Unicode
import Data.Maybe (fromJust)



-- Alternative interpration of what it means for some parameter to be classified with a set of datasets.
hecker2 :: (Ord d, Ord p) => Procedure p d -> (p -> Set d, OrderedSet (Set d))
hecker2 pr@(Procedure { input, output, includes, influences}) = (includes,  (powerset (datasets pr), (⊇)) )


greiner2 :: (Ord d, Ord p) => Procedure p d -> [(p -> LowHigh, OrderedSet LowHigh)]
greiner2 pr@(Procedure { input, output, includes, influences}) =
    [ ((\p -> if (d ∈ includes p) then Low else High), lowhigh) | d <- toList $ datasets pr]


h2IsG2 :: Procedure Parameter Datasets -> Bool
h2IsG2 = hecker2IsGreiner2

hecker2IsGreiner2 :: (Ord d, Ord p) => Procedure p d -> Bool
hecker2IsGreiner2 p = secure hecker2 p ↔ secure greiner2 p

