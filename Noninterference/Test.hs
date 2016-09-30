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
  quickCheckWith stdArgs { maxSuccess = 10000 }
             (γIsMonotone'                                   :: Component Parameter -> Specification Parameter Datasets -> Specification Parameter Datasets -> Property)
  quickCheckWith stdArgs { maxSuccess = 10000 }
             (γIsMonotone                                    :: Component Parameter -> Specification Parameter Datasets -> Specification Parameter Datasets -> Property)
  quickCheckWith stdArgs { maxSuccess = 10000 }
             (γIsMonotone                                    :: Component Parameter -> Specification Parameter Datasets -> Specification Parameter DatasetDatabase -> Property)
  quickCheckWith stdArgs { maxSuccess = 10000 }
             (γIsMonotone                                    :: Component Parameter -> Specification Parameter DatasetDatabase -> Specification Parameter Datasets -> Property)
  quickCheckWith stdArgs { maxSuccess = 10000 }
             (αIsMonotone                                    :: Component Parameter -> Implementation Parameter -> Implementation Parameter -> Property)
  quickCheckWith stdArgs { maxSuccess = 25000 }
             (weakerThanIffSecure                            :: Component Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Bool)
  quickCheckWith stdArgs { maxSuccess = 25000 }
             (galoisAlphaGamma                               :: Component Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Bool)
  quickCheckWith stdArgs { maxSuccess = 25000 }
             (galoisAlphaGamma                               :: Component Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Bool)
  quickCheckWith stdArgs { maxSuccess = 25000 }
             (weakerStongerIsNaively                          :: Component Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Bool)
  quickCheck (joanaIsKey                                      :: Component Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Bool)
  quickCheck (secureCharactization                            :: Component Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (weakerAreWeakenings                             :: Component Parameter -> Specification Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheck (weakeningsAreWeaker                             :: Component Parameter -> Specification Parameter Datasets -> Bool)
  quickCheck (weakeningsAreSafe                               :: Component Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Property)
  quickCheckWith stdArgs { maxDiscardRatio = 400 }
             (isStrongerThanIsJustified                       :: Component Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheck (isStrongerThanIsHasFewerFlowsThan               :: Component Parameter -> Specification Parameter Datasets -> Specification  Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 200 }
             (isStrongerThanIsBetterThanIsNaivelyStrongerThan :: Component Parameter -> Specification Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheck (mostPreciseIsSecure                             :: Component Parameter -> Implementation Parameter -> Bool)
  quickCheck (γMostPreciseIsMostPrecuse                       :: Component Parameter -> Implementation Parameter -> Bool)
  quickCheck (fewerFlowsIffSecure                             :: Component Parameter -> Implementation Parameter -> Specification  Parameter Datasets -> Bool)
  quickCheck (γIsγ'                                           :: Component Parameter -> Specification Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxSuccess = 10000 }
             (isStrongerThanBetterThanConsistentRelabelingRevFor :: Component Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (existsConsistentRelabelingRevIsJustified :: Component Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)
  quickCheck (relabeleingsRevAreStrongerThan (fromList allValues :: Set Datasets) :: Component Parameter -> Specification  Parameter DatasetDatabase -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeIsValid :: Component Parameter -> Implementation Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheckWith  stdArgs { maxSuccess = 1000, maxDiscardRatio = 400 }
             (strongestValidGuaranteeExtensiveIsExtensive :: Component Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeExtensiveIsValid :: Component Parameter -> Implementation Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeExtensiveIsIdempotent :: Component Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeIsIdempotent :: Component Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Bool)

impracticalProperties = do
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (secureWeakeningsAreSecure                      :: Component Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Specification  Parameter Datasets -> Property)

failingProperties = do
  quickCheckWith stdArgs { maxSuccess = 25000 }
             (galoisBetaGamma                               :: Component Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeIsExtensive :: Component Parameter -> Specification  Parameter Datasets -> Specification  Parameter Datasets -> Property)
  quickCheck (existsConsistentRelabelingIsJustified :: Component Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)

  quickCheck (existsConsistentRelabelingIsComplete :: Component Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)
  quickCheck (existsConsistentRelabelingRevIsComplete :: Component Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)
  quickCheck (isStrongerThanIsComplete                :: Component Parameter -> Implementation Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)

  quickCheck (consistentRelabelingRevForBetterThanIsStrongerThan :: Component Parameter -> Specification Parameter DatasetDatabase -> Specification  Parameter Datasets -> Property)

