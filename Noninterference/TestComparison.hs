module Noninterference.TestComparison where

import Noninterference.Procedure
import Noninterference.Testgen
import Noninterference
import Noninterference.Util
import Noninterference.Comparison
import Test.QuickCheck hiding (output)

import Data.Set
import qualified Data.Map as M

main = checkAllProperties

checkAllProperties = do
  quickCheckWith  stdArgs { maxSuccess = 10000 }
             (conversionEquivalent1                             :: Component Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxSuccess = 10000 }
             (conversionEquivalent2                             :: Component Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxSuccess = 10000 }
             (conversionEquivalent3                             :: Component Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Bool)
  quickCheckWith  stdArgs { maxSuccess = 10000 }
             (conversionEquivalent4                             :: Component Parameter -> Implementation Parameter -> Specification Parameter Datasets -> Bool)
