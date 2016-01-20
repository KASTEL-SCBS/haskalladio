module Noninterference.Test where

import Noninterference.Procedure
import Noninterference.Testgen
import Noninterference
import Test.QuickCheck


checkAllProperties = do
  quickCheck (heckerIsGreiner                                 :: Procedure Parameter Datasets -> Bool)
  quickCheck (weakeningsAreWeaker                             :: Procedure Parameter Datasets -> Bool)
  quickCheck (weakeningsAreSafe                               :: Procedure Parameter Datasets -> Property)
  quickCheckWith stdArgs { maxDiscardRatio = 400 }
             (isStrongerThanIsJustified                       :: SpecificationPair Parameter Datasets Datasets -> Property)
  quickCheck (isStrongerThanIsHasFewerFlowsThan               :: SpecificationPair Parameter Datasets Datasets -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 200 }
             (isStrongerThanIsBetterThanIsNaivelyStrongerThan :: SpecificationPair Parameter Datasets Datasets -> Property)
  quickCheck (mostPreciseIsSecure                             :: Procedure Parameter Datasets -> Bool)
  quickCheck (γMostPreciseIsMostPrecuse                       :: Procedure Parameter Datasets -> Bool)
  quickCheck (fewerFlowsIffSecure                             :: Procedure Parameter Datasets -> Bool)
  quickCheck (γIsγ'                                           :: Procedure Parameter Datasets -> Bool)



-- these are impractical to test:
--  quickCheck (isStrongerThanIsJustifiedUntestable :: Procedure Parameter Datasets -> Procedure Parameter Datasets -> Property)
--  quickCheck (secureWeakeningsAreSecure :: Procedure Parameter Datasets -> Procedure Parameter Datasets -> Property)



