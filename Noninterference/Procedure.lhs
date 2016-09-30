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

A Component `pr :: Component p`  is described by

* `input  pr:     ` the set of input  parameters (of type `p`)
* `output pr:     ` the set of output parameters (of type `p`)
\begin{code}
data Component p = Component {
    input :: Set p,
    output :: Set p
  }
\end{code}

A Component `pr` can have implementations `impl :: Implementation `, which are described by

* `influences impl: ` the set of output-parameters `pr` may influence, which is our simplified idea of the procedures implementation
\begin{code}
data Implementation p = Implementation {
    influences :: p ->  (Set p)
  }
\end{code}

A Component `pr` can have information-flow specifications `sp`, which consists of

* `includes   sp: ` the procedures parameter classifcation, as set of datasets (of type `d`)

which may or may not be fullfilled by an implementation `impl` of `pr` the implementation `influences`.
\begin{code}
data Specification p d = Specification {
    includes :: p -> (Set d),
    datasets :: Set d
  }
\end{code}



%if False
\begin{code}
instance (Show p, Ord p, Enumerable p) =>  Show (Component p) where
  show (Component { input, output }) =
    "Procedure { input = " ++ (show input) ++ ", output = " ++ (show output) ++  " }"

instance (Show p, Ord p, Enumerable p) =>  Show (Implementation p) where
  show (Implementation { influences }) =
    "Implementation { influences = (M.!) $ M." ++ (showMapFun $ influences) ++ " }"

instance (Show p, Show d, Ord p, Ord d, Enumerable p) =>  Show (Specification p d) where
  show (Specification { includes, datasets }) =
    "Specification { includes = (M.!) $ M." ++ (showMapFun includes) ++  ", datasets = " ++ (show datasets) ++ " }"
\end{code}

%endif

-- \begin{code}
-- datasets :: (Ord d, Ord p) => SpecificationProcedure p d -> Set d
-- datasets (Component { input, output, includes, influences}) = S.fromList [ d | p <- S.toList $ output ∪ input, d <- S.toList $ includes p]
-- \end{code}
