module Explore where

import Prettyprint hiding (main)
import Data.Tree


file = "queries-justify.result"
descriptionFile = "descriptions.result"
trees = fromFile file
descs = descriptionFromFile descriptionFile
root = Node (Assertion $ Node "vulnerability" []) trees 


explore is  = showNode descs is trees

main = do
    showNode descs [] trees
    putStrLn "Use, e.g., \"explore [(0,0)]\" to explore the proofs."
