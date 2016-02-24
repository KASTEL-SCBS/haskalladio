module Noninterference.TestGreiner where

import Noninterference.Procedure
import Noninterference.Testgen
import Instances.PaperExample.ExampleOne.Noninterference
import NoninterferenceGreiner
import Noninterference.Util
import Test.QuickCheck hiding (output)

import Data.Set
import qualified Data.Map as M

checkAllProperties = do
  quickCheck (joanaIsKey                                      :: Procedure Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (weakerAreWeakenings                             :: SpecificationPair Parameter Datasets Datasets -> Property)
  quickCheck (weakeningsAreWeaker                             :: Procedure Parameter Datasets -> Bool)
  quickCheck (weakeningsAreSafe                               :: Procedure Parameter Datasets -> Property)
  quickCheckWith stdArgs { maxDiscardRatio = 400 }
             (isStrongerThanIsJustifiedTestable               :: SpecificationPair Parameter Datasets Datasets -> Property)
  quickCheck (isStrongerThanIsHasFewerFlowsThan               :: SpecificationPair Parameter Datasets Datasets -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 200 }
             (isStrongerThanIsBetterThanIsNaivelyStrongerThan :: SpecificationPair Parameter Datasets Datasets -> Property)
  quickCheck (mostPreciseIsSecure                             :: Procedure Parameter Datasets -> Bool)
  quickCheck (γMostPreciseIsMostPrecuse                       :: Procedure Parameter Datasets -> Bool)
  quickCheck (fewerFlowsIffSecure                             :: Procedure Parameter Datasets -> Bool)

  quickCheckWith  stdArgs { maxSuccess = 10000 }
             (isStrongerThanBetterThanConsistentRelabelingRevForTestable :: SpecificationPair Parameter DatasetDatabase Datasets ->  Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (existsConsistentRelabelingRevIsJustifiedTestable :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)
  quickCheck (relabeleingsRevAreStrongerThan (fromList allValues :: Set Datasets) :: Procedure Parameter DatasetDatabase -> Bool)


impracticalProperties = do
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (secureWeakeningsAreSecure'                      :: SpecificationPair Parameter Datasets Datasets -> Property)


failingProperties = do
  quickCheck (existsConsistentRelabelingIsJustifiedTestable   :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)

  quickCheck (existsConsistentRelabelingIsCompleteTestable    :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)
  quickCheck (existsConsistentRelabelingRevIsCompleteTestable :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)
  quickCheck (isStrongerThanIsCompleteTestable                :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)

  quickCheck (consistentRelabelingRevForBetterThanIsStrongerThanTestable :: SpecificationPair Parameter Datasets Datasets -> Bool)
