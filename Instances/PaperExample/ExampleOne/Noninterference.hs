{-# LANGUAGE ScopedTypeVariables #-}
module Instances.PaperExample.ExampleOne.Noninterference where

-- import Misc

import qualified Palladio as P
import Security
import Noninterference

import Instances.PaperExample.ExampleOne.Palladio
import Instances.PaperExample.ExampleOne.Security


import Data.Set as S
import Data.Set.Unicode

data DatasetDatabase = Time
                     | Data
                     deriving (Show, Eq, Ord, Enum, Bounded)


relabeling :: DataSet ExampleOne -> Set DatasetDatabase
relabeling ConsumptionData = fromList [Time,Data]
relabeling _ = fromList []


isConsistentRelabelingFor :: forall d d' p. Ord d => (d' -> Set d) -> Procedure p d -> Procedure p d' -> Bool
isConsistentRelabelingFor g0 pr pr' =  pr'Relabeled `isWeakerThan` pr
  where g :: Set d' -> Set d
        g = gFrom g0
        pr'Relabeled :: Procedure p d
        pr'Relabeled = pr' { includes = \p -> g (includes pr' p) }

intersections :: (Ord a, Enum a, Bounded a) => [Set a] -> Set a
intersections = Prelude.foldl S.intersection (fromList allValues)

lowerAdjoint :: (Bounded d', Enum d', Ord d', Ord d) => (Set d' -> Set d) -> (Set d -> Set d')
lowerAdjoint g ds = intersections [ ds' | ds' <- toList $ powerset (fromList allValues), ds ⊆ g ds' ]

upperAdjoint :: (Bounded d, Enum d, Ord d', Ord d) => (Set d -> Set d') -> (Set d' -> Set d)
upperAdjoint f ds' = unions [ ds | ds <- toList $ powerset (fromList allValues), f ds ⊆ ds' ]

gFrom g0 ds' = unions [ g0 d' | d' <- toList ds']


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
--    includes (Output (Return GetValues)) = S.fromList $ [Time,Data]
    includes (Return GetHighestValue) = S.fromList $ [Data]

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

