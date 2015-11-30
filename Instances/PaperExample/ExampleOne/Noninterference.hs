{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Instances.PaperExample.ExampleOne.Noninterference where

-- import Misc

import qualified Palladio as P
import Security
import Noninterference

import Instances.PaperExample.ExampleOne.Palladio
import Instances.PaperExample.ExampleOne.Security


import Data.Set as S
import Data.Set.Unicode

instance Enumerable (P.Parameter ExampleOne) where
  allValues =     [Timestamp, Value, Start, End ]
              ++  [ Return s | s <- allValues, s /= StoreValue ]

data DatasetDatabase = Time
                     | Data
                     deriving (Show, Eq, Ord, Enum, Bounded)


relabeling :: DataSet ExampleOne -> Set DatasetDatabase
relabeling ConsumptionData = fromList [Time,Data]
relabeling _ = fromList []


getHighestValue :: Procedure (P.Parameter ExampleOne) DatasetDatabase
getHighestValue = Procedure {
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

getHighestValue' :: Procedure (P.Parameter ExampleOne) (DataSet ExampleOne)
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


getValue :: Procedure (P.Parameter ExampleOne) DatasetDatabase
getValue = Procedure {
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

getValue' :: Procedure (P.Parameter ExampleOne) (DataSet ExampleOne)
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


