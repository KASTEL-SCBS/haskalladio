module Unicode where

import Prelude hiding (elem)
import Algebra.Lattice
import Data.Foldable
import Data.List (filter)
import qualified Data.Set as S
import Data.Set.Unicode hiding ((∈))
import Data.Bool.Unicode as Bool

import Noninterference.Util (intersections)

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

(⋃) :: (Ord a) => [S.Set a] -> S.Set a
(⋃) = S.unions

(⋂) :: (Ord a, Enum a, Bounded a) => [S.Set a] -> S.Set a
(⋂) = intersections

𝝁 :: (Eq a, BoundedJoinSemiLattice a) => (a -> a) -> a
𝝁 = lfp

𝝂 :: (Eq a, BoundedMeetSemiLattice a) => (a -> a) -> a
𝝂 = gfp

(㎲⊒) :: (Eq a, BoundedJoinSemiLattice a) => a -> (a -> a) -> a
(㎲⊒) = lfpFrom

(∀) :: (Foldable t) => t a -> (a -> Bool) -> Bool
(∀) a pred = null $ filter (not.pred) $ toList a

(∃) :: (Foldable t) => t a -> (a -> Bool) -> Bool
(∃) a pred = (not.null) $ filter pred $ toList a

(→) :: Bool -> Bool -> Bool
(→) = implies
  where implies a b = (not a) || b

infix 4 ↔
(↔) :: Bool -> Bool -> Bool
(↔) = (==)


infix 4 ⇔
(⇔) :: Bool -> Bool -> Bool
(⇔) = (==)
