{-# LANGUAGE CPP #-}
-- {-# LANGUAGE DataKinds #-}
module Instances.PaperExample.ExampleOne.Queries where

import Palladio
import Instances.PaperExample.ExampleOne.Palladio
import Security
import Reasons

import Data.Tree(drawTree)

import Control.Monad.Trans.Writer.Lazy
import Control.Monad(forM_, sequence_)


#define ASTRACT_ANALYSIS
#ifdef ASTRACT_ANALYSIS
import AbstractAnalysis
#endif


import Instances.PaperExample.ExampleOne.Security
-- import Instances.PaperExample.ExampleOne.SimpleLinkModel
import Instances.PaperExample.ExampleOne.SimpleLinkModelWithExceptions
-- import Instances.PaperExample.ExampleOne.TamperableLinkModel
-- import Instances.PaperExample.ExampleOne.ComplexLinkModel

--import Instances.PaperExample.ExampleOne.InterfaceUsageExplicit
import InterfaceUsageImplicitByLocation
--import Instances.PaperExample.ExampleOne.InterfaceUsageByGroup

import Misc
import Data.Set.Monad as Set

#ifdef ABSTRACT_ANALYSIS
query1 :: [(Attacker ExampleOne, Set (Parameter ExampleOne))]
query1 = [(attacker, accessibleParameters attacker) | attacker <- allValues ]
#endif

wellFormedExampleOne = fst $ (wellformed :: (Bool, Attacker ExampleOne))

query2 :: [(Attacker ExampleOne, Set (ResourceContainer ExampleOne,[Reason ExampleOne]))]
query2 = [(attacker, runWriterT $ containersFullyAccessibleBy attacker) | attacker <- allValues ]

query3 :: [(Attacker ExampleOne, Set ((LinkingResource ExampleOne, DataSet ExampleOne), [Reason ExampleOne]))]
query3 = [(attacker, runWriterT $ linksDataAccessibleBy attacker) | attacker <- allValues ]




query5 :: [(Attacker ExampleOne, Set (DataSet ExampleOne, [Reason ExampleOne]))]
query5 = [(attacker, runWriterT $ dataAccessibleTo attacker) | attacker <- allValues ]

query6 :: [(Attacker ExampleOne, Set (Insecure,[Reason ExampleOne]))]
query6 = [(attacker, runWriterT $ isInSecureWithRespectTo attacker) | attacker <- allValues ]

query7 :: [(Attacker ExampleOne, Set (DataSet ExampleOne))]
query7 = [(attacker, dataAllowedToBeAccessedBy attacker) | attacker <- allValues ]

woReasons :: [(a, Set (b,[Reason ExampleOne]))] -> [(a, Set b)]
woReasons = fmap (\(a,s) -> (a, fmap fst s))

pretty :: Show t => [t] -> IO ()
pretty list  = putStrLn $ showByLine $ list

prettyReasons :: Show a => [(a, Set (b,[Reason ExampleOne]))] -> IO ()
prettyReasons list = putStrLn $ showByLine $ fmap pretty list
  where pretty (a, reasons) = (a, fmap ((fmap (drawTree . toTree)) . snd) reasons)


prettyReasonsM :: (Show b, Ord b, Show a) => [(a, Set (b,[Reason ExampleOne]))] -> IO ()
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
