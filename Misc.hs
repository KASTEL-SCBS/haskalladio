module Misc where

import Data.Set as Set

(∈) :: Ord a => a -> Set a -> Bool
(∈) = Set.member

(∆) :: Set a -> [a]
(∆) = Set.elems

(⋅) :: Set a -> [a]
(⋅) = Set.elems


showByLine :: Show t => [t] -> String
showByLine set = Prelude.foldr (\x lines -> (show x) ++ "\n" ++ lines) [] set