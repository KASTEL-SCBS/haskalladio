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

showByLine :: Show t => [t] -> String
showByLine set = Prelude.foldr (\x lines -> (show x) ++ "\n" ++ lines) [] set