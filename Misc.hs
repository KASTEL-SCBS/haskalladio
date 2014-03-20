module Misc where

--import Data.Set

showByLine :: Show t => [t] -> String
showByLine set = foldr (\x lines -> (show x) ++ "\n" ++ lines) [] set