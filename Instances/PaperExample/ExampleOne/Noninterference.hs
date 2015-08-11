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

intersections :: (Ord a, Enum a, Bounded a) => [Set a] -> Set a
intersections = Prelude.foldl S.intersection (fromList allValues)

rofl :: (DataSet ExampleOne -> Set DatasetDatabase) -> (Set DatasetDatabase -> Set (DataSet ExampleOne))
rofl g ds = intersections [ ds' | ds' <- toList $ powerset (fromList allValues), ds ⊆ gAll ds' ]
  where gAll ds' = unions [ g d' | d' <- toList ds']

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

-- getHighestValue' :: Procedure (P.Parameter ExampleOne) (DataSet ExampleOne)
-- getHighestValue' = getHighestValue {
--     includes = \p -> rofl relabeling (includes getHighestValue p)
--   }





-- getValue :: Procedure (P.Parameter ExampleOne) DatasetDatabase
-- getValue = Procedure {
--       input  = S.fromList [Timestamp, Value, Start, End],
--       output = S.fromList [Return GeValue],
--       includes = includes,
--       influences = influences
--     }
--   where
--     includes Timestamp = S.fromList $ [Time]
--     includes Value     = S.fromList $ [Data]
--     includes Start     = S.fromList $ [Time]
--     includes End       = S.fromList $ [Time]
-- --    includes (Output (Return GetValues)) = S.fromList $ [Time,Data]
--     includes (Return GetHighestValue) = S.fromList $ [Data]

--     influences Timestamp = S.fromList [Return GetValue]
--     influences Value     = S.fromList [Return GetValue]
--     influences Start     = S.fromList [Return GetValue]
--     influences End       = S.fromList [Return GetValue]


--     influences _         = S.empty
