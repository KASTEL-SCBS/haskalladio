{-# LANGUAGE NamedFieldPuns #-}
module Noninterference.Comparison where
import Unicode
import qualified Noninterference        as NI
import qualified NoninterferenceGreiner as NIG
import Noninterference.Util
import Noninterference.Component

import Data.Set as S
import Data.Set.Unicode
import Data.List
import Data.Char

import qualified Data.Map as M

import Test.QuickCheck hiding (output)

-- Conversion from Greiner-Style (ii) specifications to Hecker-Style (i), using parameters as datsets
fromGreiner :: (Ord p, Ord d) => Component p -> Specification p d -> Specification p p
fromGreiner co sp = (NI.α co) . (NIG.γ co) $ sp


-- Conversion from Hecker-Style (i) specifications to Greiner-Style (ii), using parameters as datsets
fromHecker :: (Ord p, Ord d) => Component p -> Specification p d -> Specification p p
fromHecker co sp = (NIG.α co) . (NI.γ co) $ sp





conversionEquivalent1 ::  (Ord p, Ord d) => Component p -> Implementation p -> Specification p d -> Bool
conversionEquivalent1 co impl sp =  NIG.secure NIG.key co impl sp ↔ NI.secure  NI.key co impl sp'
  where sp' = fromGreiner co sp

conversionEquivalent2 ::  (Ord p, Ord d) => Component p -> Implementation p -> Specification p d -> Bool
conversionEquivalent2 co impl sp =  NI.secure NI.key co impl sp ↔ NIG.secure  NIG.key co impl sp'
  where sp' = fromHecker co sp

conversionEquivalent3 ::  (Ord p, Ord d) => Component p -> Implementation p -> Specification p d -> Bool
conversionEquivalent3 co impl sp =  NIG.secure NIG.joana co impl sp ↔ NI.secure  NI.joana co impl sp'
  where sp' = fromGreiner co sp

conversionEquivalent4 ::  (Ord p, Ord d) => Component p -> Implementation p -> Specification p d -> Bool
conversionEquivalent4 co impl sp =  NI.secure NI.joana co impl sp ↔ NIG.secure  NIG.joana co impl sp'
  where sp' = fromHecker co sp

