{-# LANGUAGE NamedFieldPuns #-}
module Noninterference.Comparison where
import Unicode
import qualified Noninterference        as NI
import qualified NoninterferenceGreiner as NIG
import Noninterference.Util
import Noninterference.Procedure

import Data.Set as S
import Data.Set.Unicode
import Data.List
import Data.Char

import qualified Data.Map as M

import Test.QuickCheck hiding (output)

-- Conversion from Greiner-Style (ii) specifications to Hecker-Style (i), using parameters as datsets
fromGreiner :: (Ord p, Ord d) => Procedure p d -> Procedure p p
fromGreiner pr = pr { includes = includes pr' }
  where pr' = NI.α . NIG.γ $ pr


-- Conversion from Hecker-Style (i) specifications to Greiner-Style (ii), using parameters as datsets
fromHecker :: (Ord p, Ord d) => Procedure p d -> Procedure p p
fromHecker pr = pr { includes = includes pr' }
  where pr' = NIG.α . NI.γ $ pr



-- fromHeckerSubset :: (Ord p, Ord d) => Procedure p d -> Procedure p (Set d)
-- fromHeckerSubset pr = pr { includes = includes pr' }
--   where pr' = NIG.α . NI.γ $ pr



conversionEquivalent1 ::  (Ord p, Ord d) => Procedure p d -> Bool
conversionEquivalent1 pr =  NIG.secure NIG.key pr ↔ NI.secure  NI.key pr'
  where pr' = fromGreiner pr

conversionEquivalent2 ::  (Ord p, Ord d) => Procedure p d -> Bool
conversionEquivalent2 pr =  NI.secure  NI.key  pr ↔ NIG.secure NIG.key pr'
  where pr' = fromHecker  pr


conversionEquivalent3 ::  (Ord p, Ord d) => Procedure p d -> Bool
conversionEquivalent3 pr =  NIG.secure NIG.joana pr ↔ NI.secure  NI.joana pr'
  where pr' = fromGreiner pr

conversionEquivalent4 ::  (Ord p, Ord d) => Procedure p d -> Bool
conversionEquivalent4 pr =  NI.secure  NI.joana  pr ↔ NIG.secure NIG.joana pr'
  where pr' = fromHecker  pr

