{-# LANGUAGE NamedFieldPuns #-}
module Noninterference.Procedure where

import Noninterference.Util

import Data.Set as S
import Data.Set.Unicode


-- A Procedure is described by
-- * influences p: the set of output-parameters p may influence
-- * includes p: its parameter classifcation, as set of datasets
--
-- "includes" hence implicitly specifies an ifc requirement, which may or may not be fullfilled by "influences".
data Procedure p d = Procedure {
    input :: Set p,
    output :: Set p,
    includes :: p -> (Set d),
    influences :: p ->  (Set p)
  }

instance (Show p, Show d, Ord p, Ord d, Enumerable p) =>  Show (Procedure p d) where
  show (Procedure { input, output, includes, influences}) =
    "Procedure { input = " ++ (show input) ++ ", output = " ++ (show output) ++ ", includes = (M.!) $ M." ++ (showMapFun includes) ++ ", influences = (M.!) $ M." ++ (showMapFun influences) ++ " }"


datasets :: (Ord d, Ord p) => Procedure p d -> Set d
datasets (Procedure { input, output, includes, influences}) = fromList [ d | p <- toList $ output ∪ input, d <- toList $ includes p]
