%if False
\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
module Noninterference.Component where

import Noninterference.Util

import qualified Data.Set as S
import Data.Set (Set)
import Unicode
import Data.Set.Unicode
\end{code}
%endif

A Component `co :: Component p`  is described by

* `input  co:     ` the set of input  parameters (of type `p`)
* `output co:     ` the set of output parameters (of type `p`)
\begin{code}
data Component p = Component {
    input :: Set p,
    output :: Set p
  }
\end{code}

A Component `co` can have implementations `impl`, which are described by

* `influences impl: ` the set of output-parameters `co` may influence, which is our simplified idea of the components implementation
\begin{code}
data Implementation p = Implementation {
    influences :: p ->  (Set p)
  }
\end{code}

A Component `co` can have information-flow specifications `sp`, which consists of

* `includes   sp: ` the components parameter classifcation, as set of datasets (of type `d`)

which may or may not be fullfilled by an implementation `impl` of `co` the implementation `influences`.
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
    "Component { input = " ++ (show input) ++ ", output = " ++ (show output) ++  " }"

instance (Show p, Ord p, Enumerable p) =>  Show (Implementation p) where
  show (Implementation { influences }) =
    "Implementation { influences = (M.!) $ M." ++ (showMapFun $ influences) ++ " }"

instance (Show p, Show d, Ord p, Ord d, Enumerable p) =>  Show (Specification p d) where
  show (Specification { includes, datasets }) =
    "Specification { includes = (M.!) $ M." ++ (showMapFun includes) ++  ", datasets = " ++ (show datasets) ++ " }"
\end{code}

%endif
