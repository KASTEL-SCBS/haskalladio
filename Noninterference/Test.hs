module Noninterference.Test where

import Noninterference.Procedure
import Noninterference.Testgen
import Instances.PaperExample.ExampleOne.Noninterference
import Noninterference
import Test.QuickCheck


checkAllProperties = do
  quickCheck (heckerIsGreiner                                 :: Procedure Parameter Datasets -> Bool)
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
  quickCheck (γIsγ'                                           :: Procedure Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxSuccess = 10000 }
             (isStrongerThanBetterThanConsistentRelabelingRevForTestable :: SpecificationPair Parameter DatasetDatabase Datasets ->  Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (existsConsistentRelabelingRevIsJustifiedTestable :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)


failingProperties = do
  quickCheck (existsConsistentRelabelingIsJustifiedTestable   :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)

  quickCheck (existsConsistentRelabelingIsCompleteTestable    :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)
  quickCheck (existsConsistentRelabelingRevIsCompleteTestable :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)
  quickCheck (isStrongerThanIsCompleteTestable                :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)

  quickCheck (consistentRelabelingRevForBetterThanIsStrongerThanTestable :: SpecificationPair Parameter Datasets Datasets -> Bool)
