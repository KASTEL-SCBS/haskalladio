%if False
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Instances.PaperExample.ExampleOne.Noninterference where

-- import Misc

import qualified Palladio as P

import Unicode
import Security
import Noninterference
import Noninterference.Util
import Noninterference.Procedure

import Instances.PaperExample.ExampleOne.Palladio
import Instances.PaperExample.ExampleOne.Security


import Data.Set as S
import Data.Set.Unicode

import qualified Data.Map as M

instance Enumerable (P.Parameter ExampleOne) where
  allValues =     [Timestamp, Value, Start, End ]
              ++  [ Return s | s <- allValues, s /= StoreValue ]
\end{code}
%endif


An alternative set of Datasets, differentiating between Time- and other Data:
\begin{code}
data DatasetDatabase = Time
                     | Data
                     deriving (Show, Eq, Ord, Enum, Bounded)
\end{code}

An implementation of `getValue` that is secure with regard to its ifc specification
in terms of "Time" and "Data":
\begin{code}
getValue :: Component (P.Parameter ExampleOne) DatasetDatabase
getValue = Component {
      input  = S.fromList [Timestamp, Value, Start, End],
      output = S.fromList [Return GetValues],
      includes = includes,
      influences = influences
    }
  where
    includes Timestamp = S.fromList $ [Time]
    includes Value     = S.fromList $ [Data]
    includes Start     = S.fromList $ [Time]
    includes End       = S.fromList $ [Time]
    includes (Return GetValues) = S.fromList $ [Time,Data]
    includes _         = S.empty

    influences Timestamp = S.fromList [Return GetValues]
    influences Value     = S.fromList [Return GetValues]
    influences Start     = S.fromList [Return GetValues]
    influences End       = S.fromList [Return GetValues]
    influences _         = S.empty
\end{code}

The same implementation, but with a different ifc specification (in terms of the datasets used in the paper):
\begin{code}
getValue' :: Component (P.Parameter ExampleOne) (DataSet ExampleOne)
getValue' = getValue {
      includes = includes
    }
  where
    includes Timestamp = S.fromList $ [ConsumptionData]
    includes Value     = S.fromList $ [ConsumptionData]
    includes Start     = S.fromList $ [ConsumptionData]
    includes End       = S.fromList $ [ConsumptionData]
    includes (Return GetValues) = S.fromList $ [ConsumptionData]
    includes _         = S.empty
\end{code}

The "natural" relabeling between those:
\begin{code}
relabeling :: DataSet ExampleOne -> Set DatasetDatabase
relabeling ConsumptionData = fromList [Time,Data]
relabeling _               = fromList []
\end{code}

All possible relabelings:
\begin{code}
relabelings :: [ DataSet ExampleOne -> Set DatasetDatabase ]
relabelings = fmap ((M.!) . (M.fromList)) [
      [ (ConsumptionData, S.fromList ds1), (PublicData, S.fromList ds2), (BillingData, S.fromList ds3) ]
   | ds1 <- subsets, ds2 <- subsets, ds3 <- subsets
  ]
 where subsets = listPowerset [Time,Data]
\end{code}

Unfortunately, there is no consistent relabeling at all:
\begin{code}
noConsistentRelabelingGetValue = (∀) relabelings (\g0 -> not (isConsistentRelabelingFor g0 getValue getValue'))
\end{code}

However, the criterion without relabeling worlds:
    getValue `isStrongerThan` getValue'
\begin{code}
isStrongerThanCriterionHoldsGetValue        = getValue        `isStrongerThan` getValue'
\end{code}

%if False
\begin{code}
noConsistentRelabelingGetHighestValue =  (∀) relabelings (\g0 -> not $ isConsistentRelabelingFor g0 getHighestValue getHighestValue')
isStrongerThanCriterionHoldsGetHighestValue = getHighestValue `isStrongerThan` getHighestValue'





getHighestValue :: Component (P.Parameter ExampleOne) DatasetDatabase
getHighestValue = Component {
      input  = S.fromList [Timestamp, Value, Start, End],
      output = S.fromList [Return GetHighestValue],
      includes = includes,
      influences = influences
    }
  where
    includes Timestamp = S.fromList $ [Time]
    includes Value     = S.fromList $ [Data]
    includes Start     = S.fromList $ [Time]
    includes End       = S.fromList $ [Time]
    includes (Return GetHighestValue) = S.fromList $ [Data]
    includes _ = S.empty

    influences Timestamp = S.fromList []
    influences Value     = S.fromList [Return GetHighestValue]
    influences Start     = S.fromList []
    influences End       = S.fromList []
    influences _         = S.empty

getHighestValue' :: Component (P.Parameter ExampleOne) (DataSet ExampleOne)
getHighestValue' = getHighestValue {
      includes = includes
    }
  where
    includes Timestamp = S.fromList $ [ConsumptionData]
    includes Value     = S.fromList $ [ConsumptionData]
    includes Start     = S.fromList $ [ConsumptionData]
    includes End       = S.fromList $ [ConsumptionData]
    includes (Return GetHighestValue) = S.fromList $ [ConsumptionData]
    includes _         = S.empty
\end{code}
%endif
