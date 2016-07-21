%if False
\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
module Noninterference.Procedure where

import Noninterference.Util

import qualified Data.Set as S
import Data.Set (Set)
import Unicode
import Data.Set.Unicode
\end{code}
%endif

A Procedure `pr :: Procedure p d`  is described by

* `input  pr:     ` the set of input  parameters (of type `p`)
* `output pr:     ` the set of output parameters (of type `p`)
* `influences pr: ` the set of output-parameters `pr` may influence, which is our simplified idea of the procedures implementation
* `includes   pr: ` its parameter classifcation, as set of datasets (of type `d`)

`includes` hence  is the ifc specification of `pr`, which may or may not be fullfilled by the implementation `influences`.
\begin{code}
data Procedure p d = Procedure {
    input :: Set p,
    output :: Set p,
    includes :: p -> (Set d),
    influences :: p ->  (Set p)
  }
\end{code}
%if False
\begin{code}
instance (Show p, Show d, Ord p, Ord d, Enumerable p) =>  Show (Procedure p d) where
  show (Procedure { input, output, includes, influences})
   | (∃) output (\o -> influences o /= (∅) ) = error "invalid procedure"
   | otherwise =
    "Procedure { input = " ++ (show input) ++ ", output = " ++ (show output) ++ ", includes = (M.!) $ M." ++ (showMapFun includes) ++ ", influences = (M.!) $ M." ++ (showMapFun $ (\p -> if (p ∈ output) then (∅) else influences p)) ++ " }"
\end{code}

%endif

\begin{code}
datasets :: (Ord d, Ord p) => Procedure p d -> Set d
datasets (Procedure { input, output, includes, influences}) = S.fromList [ d | p <- S.toList $ output ∪ input, d <- S.toList $ includes p]
\end{code}
