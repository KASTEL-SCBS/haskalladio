module Noninterference.Test where

import Noninterference.Procedure
import Noninterference.Testgen
import Noninterference
import Noninterference.Util
import Test.QuickCheck hiding (output)

import Noninterference.Export

import Unicode

import Data.Set
import qualified Data.Map as M

data DatasetDatabase = Time
                     | Data
                     deriving (Show, Eq, Ord, Enum, Bounded)

main = checkAllProperties

checkAllProperties = do
  quickCheckWith stdArgs { maxSuccess = 25000 }
             (weakerThanIffSecure                            :: Procedure Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Bool)
  quickCheckWith stdArgs { maxSuccess = 25000 }
             (galoisAlphaGamma                               :: Procedure Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Bool)
  quickCheckWith stdArgs { maxSuccess = 25000 }
             (galoisAlphaGamma                               :: Procedure Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Bool)
  quickCheckWith stdArgs { maxSuccess = 25000 }
             (weakerStongerIsNaively                          :: Procedure Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Bool)
  quickCheck (joanaIsKey                                      :: Procedure Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Bool)
  quickCheck (secureCharactization                            :: Procedure Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (weakerAreWeakenings                             :: Procedure Parameter -> Specification Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheck (weakeningsAreWeaker                             :: Procedure Parameter -> Specification Parameter Datasets -> Bool)
  quickCheck (weakeningsAreSafe                               :: Procedure Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Property)
  quickCheckWith stdArgs { maxDiscardRatio = 400 }
             (isStrongerThanIsJustified                       :: Procedure Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheck (isStrongerThanIsHasFewerFlowsThan               :: Procedure Parameter -> Specification Parameter Datasets -> Specification  Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 200 }
             (isStrongerThanIsBetterThanIsNaivelyStrongerThan :: Procedure Parameter -> Specification Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheck (mostPreciseIsSecure                             :: Procedure Parameter -> Implementation Parameter -> Bool)
  quickCheck (γMostPreciseIsMostPrecuse                       :: Procedure Parameter -> Implementation Parameter -> Bool)
  quickCheck (fewerFlowsIffSecure                             :: Procedure Parameter -> Implementation Parameter -> Specification  Parameter Datasets -> Bool)
  quickCheck (γIsγ'                                           :: Procedure Parameter -> Specification Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxSuccess = 10000 }
             (isStrongerThanBetterThanConsistentRelabelingRevFor :: Procedure Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (existsConsistentRelabelingRevIsJustified :: Procedure Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)
  quickCheck (relabeleingsRevAreStrongerThan (fromList allValues :: Set Datasets) :: Procedure Parameter -> Specification  Parameter DatasetDatabase -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeIsValid :: Procedure Parameter -> Implementation Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheckWith  stdArgs { maxSuccess = 1000, maxDiscardRatio = 400 }
             (strongestValidGuaranteeExtensiveIsExtensive :: Procedure Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeExtensiveIsValid :: Procedure Parameter -> Implementation Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeExtensiveIsIdempotent :: Procedure Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeIsIdempotent :: Procedure Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Bool)

impracticalProperties = do
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (secureWeakeningsAreSecure                      :: Procedure Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Specification  Parameter Datasets -> Property)

failingProperties = do
  quickCheckWith stdArgs { maxSuccess = 25000 }
             (galoisBetaGamma                               :: Procedure Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeIsExtensive :: Procedure Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheck (existsConsistentRelabelingIsJustified :: Procedure Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)

  quickCheck (existsConsistentRelabelingIsComplete :: Procedure Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)
  quickCheck (existsConsistentRelabelingRevIsComplete :: Procedure Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)
  quickCheck (isStrongerThanIsComplete                :: Procedure Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)

  quickCheck (consistentRelabelingRevForBetterThanIsStrongerThan :: Procedure Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)

