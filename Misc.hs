module Misc where

import Data.Set.Monad as Set

import Control.Monad(MonadPlus, mplus)

-- TODO: Use Data.Set.Unicode
(∈) :: Ord a => a -> Set a -> Bool
(∈) = Set.member

-- (⋅) :: Ord a => Set a -> [a]
-- (⋅) = Set.elems

(⊆) :: Ord a => Set a -> Set a -> Bool
(⊆) = Set.isSubsetOf


(⊇) :: Ord a => Set a -> Set a -> Bool
(⊇) = flip Set.isSubsetOf

(∪) :: Ord α => Set α -> Set α -> Set α
(∪) = union

(⊔) :: MonadPlus m =>  m a -> m a -> m a
(⊔) = mplus

(∩) :: Ord α => Set α -> Set α -> Set α
(∩) = intersection

isEmpty :: Ord a => Set a -> Bool
isEmpty = Set.null

implies a b = (not a) || b
(→) :: Bool -> Bool -> Bool
(→) = implies

𝝁 :: (Ord a) => (Set a -> Set a) -> Set a
𝝁 = smallest

(㎲⊒) :: (Ord a) => Set a -> (Set a -> Set a) -> Set a
(㎲⊒) = smallestContaining



smallestContaining :: (Ord a) => Set a -> (Set a -> Set a) -> Set a
smallestContaining s f = fixiter s where
 fixiter current
  | next    == current = current
  | otherwise          = fixiter next
  where next = f current

smallest :: (Ord a) => (Set a -> Set a) -> Set a
smallest = smallestContaining Set.empty


(∀) :: (Eq a, Enum a, Bounded a, Ord a) => (a -> Bool) -> Bool
(∀) pred = isEmpty $ Set.filter (not.pred) (fromList allValues)

(∃) :: (Eq a, Enum a, Bounded a, Ord a) => (a -> Bool) -> Bool
(∃) pred = (not.isEmpty) $ Set.filter pred (fromList allValues)


(∀∈) :: [a] -> (a -> Bool) -> Bool
(∀∈) = flip all

(∃∈) :: [a] -> (a -> Bool) -> Bool
(∃∈) = flip any

class All a where
  allValues' :: [a]

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

data Lol = A | B deriving (Bounded, Enum, Ord, Eq)
rofl = (∃) tt
  where tt :: Lol -> Bool
        tt x = True
