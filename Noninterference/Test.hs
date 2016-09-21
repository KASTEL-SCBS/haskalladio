module Noninterference.Test where

import Noninterference.Procedure
import Noninterference.Testgen
import Noninterference
import Noninterference.Util
import Test.QuickCheck hiding (output)


import Control.Monad.Random (getStdRandom, randomR)
import System.Process (runInteractiveCommand, system)
import Noninterference.Export


import Data.Set
import qualified Data.Map as M

data DatasetDatabase = Time
                     | Data
                     deriving (Show, Eq, Ord, Enum, Bounded)

main = checkAllProperties

checkAllProperties = do
  quickCheckWith stdArgs { maxSuccess = 25000 }
             (weakerStongerIsNaively                          :: Procedure Parameter Datasets -> Procedure Parameter Datasets -> Bool)
  quickCheck (joanaIsKey                                      :: Procedure Parameter Datasets -> Bool)
  quickCheck (secureCharactization                            :: Procedure Parameter Datasets -> Bool)
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
  quickCheck (γIsγ'                                           :: Procedure Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxSuccess = 10000 }
             (isStrongerThanBetterThanConsistentRelabelingRevForTestable :: SpecificationPair Parameter DatasetDatabase Datasets ->  Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (existsConsistentRelabelingRevIsJustifiedTestable :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)
  quickCheck (relabeleingsRevAreStrongerThan (fromList allValues :: Set Datasets) :: Procedure Parameter DatasetDatabase -> Bool)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeIsValid :: Procedure Parameter Datasets -> Procedure Parameter Datasets -> Property)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeExtensiveIsExtensive :: Procedure Parameter Datasets -> Procedure Parameter Datasets -> Property)
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeExtensiveIsValid :: Procedure Parameter Datasets -> Procedure Parameter Datasets -> Property)

impracticalProperties = do
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (secureWeakeningsAreSecure'                      :: SpecificationPair Parameter Datasets Datasets -> Property)


failingProperties = do
  quickCheckWith  stdArgs { maxDiscardRatio = 400 }
             (strongestValidGuaranteeIsExtensive :: Procedure Parameter Datasets -> Procedure Parameter Datasets -> Property)
  quickCheck (existsConsistentRelabelingIsJustifiedTestable   :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)

  quickCheck (existsConsistentRelabelingIsCompleteTestable    :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)
  quickCheck (existsConsistentRelabelingRevIsCompleteTestable :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)
  quickCheck (isStrongerThanIsCompleteTestable                :: SpecificationPair Parameter DatasetDatabase Datasets -> Property)

  quickCheck (consistentRelabelingRevForBetterThanIsStrongerThanTestable :: SpecificationPair Parameter Datasets Datasets -> Bool)



showProcedure pr = do
  let tex = toTikzComplete pr
  randomInt <- getStdRandom (randomR (1,65536)) :: IO Int
  let file = "tmpfile" ++ (show randomInt)
  writeFile (file ++ ".tex")  tex
  system                $ "pdflatex -interaction=batchmode " ++ (file ++ ".tex")
  runInteractiveCommand $ "evince " ++ (file ++ ".pdf")

