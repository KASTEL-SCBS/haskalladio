module Explore where

import Prettyprint hiding (main)
import Data.Tree
import Control.Monad(forM_)


file = "queries-justify.result"
descriptionFile = "descriptions.result"
worlds =[ (world, proofs) | (world, proofs) <- fromFile file]
trees = [          proofs | (world, proofs) <- worlds ]
descs = descriptionFromFile descriptionFile


explore w is  = do
    putStrLn $ ""
    putStr   $ "World " ++ (show w) ++ ":  "
    putStrLn $ (showWorld world)
    showNode descs is proofs
    putStrLn $ ""
  where (world, proofs) = worlds !! w

main = do
    forM_ (zip [0..] worlds) (\(i,(world, proofs)) -> do
        putStrLn $ ""
        putStr   $ "World " ++ (show i) ++ ":  "
        putStrLn $ (showWorld world)
        showNode descs [] proofs
        putStrLn $ ""
     )
    putStrLn "Use, e.g., \"explore 0 [(0,0)]\" to explore the proofs in world 0"
