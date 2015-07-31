module Queries where

import Data.Tree(drawTree)
import Control.Monad(forM_, sequence_)

import Data.Set.Monad as Set
import Reasons

woReasons :: [(a, Set (b,[Reason m]))] -> [(a, Set b)]
woReasons = fmap (\(a,s) -> (a, fmap fst s))

pretty :: Show t => [t] -> IO ()
pretty list  = putStrLn $ showByLine $ list

prettyReasons :: Show a => [(a, Set (b,[Reason m]))] -> IO ()
prettyReasons list = putStrLn $ showByLine $ fmap pretty list
  where pretty (a, reasons) = (a, fmap ((fmap (drawTree . toTree)) . snd) reasons)


prettyReasonsM :: (Show b, Ord b, Show a) => [(a, Set (b,[Reason m]))] -> IO ()
prettyReasonsM list =
    forM_ (fmap pretty list)
      (\(a,pairs) -> do putStr ("(" ++ (show a) ++ ", ")
                        forM_ pairs (\(b,reasons) ->
                           do putStr ("(" ++ (show b) ++ ", ")
                              forM_ reasons putStrLn
                              putStrLn ")"
                         )
                        putStrLn ")"
                      )
 where pretty (a, pairs) = (a, toList $ fmap (\(b, reasons) ->
                                               (b, fmap (drawTree . toTree) reasons))
                                        pairs)

showByLine :: Show t => [t] -> String
showByLine set = Prelude.foldr (\x lines -> (show x) ++ "\n" ++ lines) [] set
