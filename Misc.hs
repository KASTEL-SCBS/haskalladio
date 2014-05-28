module Misc where

import Data.Set as Set

-- TODO: Use Data.Set.Unicode
(∈) :: Ord a => a -> Set a -> Bool
(∈) = Set.member

(∆) :: Set a -> [a]
(∆) = Set.elems

(⋅) :: Set a -> [a]
(⋅) = Set.elems

(⊆) :: Ord a => Set a -> Set a -> Bool
(⊆) = Set.isSubsetOf

(∪) :: Ord α => Set α -> Set α -> Set α
(∪) = union

(∩) :: Ord α => Set α -> Set α -> Set α
(∩) = intersection

isEmpty :: Set a -> Bool
isEmpty = Set.null

implies a b = (not a) || b
(→) :: Bool -> Bool -> Bool
(→) = implies

μ :: (Eq a) => (Set a -> Set a) -> Set a
μ = smallest


smallest :: (Eq a) => (Set a -> Set a) -> Set a
smallest f = fixiter Set.empty where
 fixiter current
  | next    == current = current
  | otherwise          = fixiter next
  where next = f current

showByLine :: Show t => [t] -> String
showByLine set = Prelude.foldr (\x lines -> (show x) ++ "\n" ++ lines) [] set

class All a where
  allValues' :: [a]

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]
