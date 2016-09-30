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
fromGreiner :: (Ord p, Ord d) => Component p -> Specification p d -> Specification p p
fromGreiner pr sp = (NI.α pr) . (NIG.γ pr) $ sp


-- Conversion from Hecker-Style (i) specifications to Greiner-Style (ii), using parameters as datsets
fromHecker :: (Ord p, Ord d) => Component p -> Specification p d -> Specification p p
fromHecker pr sp = (NIG.α pr) . (NI.γ pr) $ sp




-- fromHeckerSubset :: (Ord p, Ord d) => Component p d -> Component p (Set d)
-- fromHeckerSubset pr = pr { includes = includes pr' }
--   where pr' = NIG.α . NI.γ $ pr



conversionEquivalent1 ::  (Ord p, Ord d) => Component p -> Implementation p -> Specification p d -> Bool
conversionEquivalent1 pr impl sp =  NIG.secure NIG.key pr impl sp ↔ NI.secure  NI.key pr impl sp'
  where sp' = fromGreiner pr sp

conversionEquivalent2 ::  (Ord p, Ord d) => Component p -> Implementation p -> Specification p d -> Bool
conversionEquivalent2 pr impl sp =  NI.secure NI.key pr impl sp ↔ NIG.secure  NIG.key pr impl sp'
  where sp' = fromHecker pr sp

conversionEquivalent3 ::  (Ord p, Ord d) => Component p -> Implementation p -> Specification p d -> Bool
conversionEquivalent3 pr impl sp =  NIG.secure NIG.joana pr impl sp ↔ NI.secure  NI.joana pr impl sp'
  where sp' = fromGreiner pr sp

conversionEquivalent4 ::  (Ord p, Ord d) => Component p -> Implementation p -> Specification p d -> Bool
conversionEquivalent4 pr impl sp =  NI.secure NI.joana pr impl sp ↔ NIG.secure  NIG.joana pr impl sp'
  where sp' = fromHecker pr sp

