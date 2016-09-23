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
data Procedure p = Procedure {
    input :: Set p,
    output :: Set p
  }

data Implementation p = Implementation {
    influences :: p ->  (Set p)
  }

data Specification p d = Specification {
    includes :: p -> (Set d),
    datasets :: Set d
  }
\end{code}



%if False
\begin{code}
instance (Show p, Ord p, Enumerable p) =>  Show (Procedure p) where
  show (Procedure { input, output }) =
    "Procedure { input = " ++ (show input) ++ ", output = " ++ (show output) ++  " }"

instance (Show p, Ord p, Enumerable p) =>  Show (Implementation p) where
  show (Implementation { influences }) =
    "Implementation { influences = (M.!) $ M." ++ (showMapFun $ influences) ++ " }"

instance (Show p, Show d, Ord p, Ord d, Enumerable p) =>  Show (Specification p d) where
  show (Specification { includes }) =
    "Procedure { includes = (M.!) $ M." ++ (showMapFun includes) ++ " }"
\end{code}

%endif

-- \begin{code}
-- datasets :: (Ord d, Ord p) => SpecificationProcedure p d -> Set d
-- datasets (Procedure { input, output, includes, influences}) = S.fromList [ d | p <- S.toList $ output ∪ input, d <- S.toList $ includes p]
-- \end{code}
