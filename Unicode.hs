module Unicode where

import Algebra.Lattice
import Data.Set as S


infixl 6 ⊔
(⊔) :: (JoinSemiLattice a) => a -> a -> a
(⊔) = join

infixl 7 ⊓
(⊓) :: (MeetSemiLattice a) => a -> a -> a
(⊓) = meet

(⊥) :: (BoundedJoinSemiLattice a) => a
(⊥) = bottom

(⊤) :: (BoundedMeetSemiLattice a) => a
(⊤) = top


(∐) :: (BoundedMeetSemiLattice a) => [a] -> a
(∐) = meets

(∏) :: (BoundedJoinSemiLattice a) => [a] -> a
(∏) = joins


𝝁 :: (Eq a, BoundedJoinSemiLattice a) => (a -> a) -> a
𝝁 = lfp

𝝂 :: (Eq a, BoundedMeetSemiLattice a) => (a -> a) -> a
𝝂 = gfp

(㎲⊒) :: (Eq a, BoundedJoinSemiLattice a) => a -> (a -> a) -> a
(㎲⊒) = lfpFrom

(∀) :: (Eq a, Ord a) => Set a -> (a -> Bool) -> Bool
(∀) a pred = S.null $ S.filter (not.pred) a


(→) :: Bool -> Bool -> Bool
(→) = implies
  where implies a b = (not a) || b

infix 4 ↔
(↔) :: Bool -> Bool -> Bool
(↔) = (==)
