{-# LANGUAGE CPP #-}
module Instances.PaperExample.ExampleOne.Queries where

import Palladio
import Instances.PaperExample.ExampleOne.Palladio
import Security
import Reasons

import Control.Monad.Trans.Writer.Lazy


#define ASTRACT_ANALYSIS
#ifdef ASTRACT_ANALYSIS
import AbstractAnalysis
#endif


import Instances.PaperExample.ExampleOne.Security
import Instances.PaperExample.ExampleOne.SimpleLinkModel
-- import Instances.PaperExample.ExampleOne.TamperableLinkModel
-- import Instances.PaperExample.ExampleOne.ComplexLinkModel


import Misc
import Data.Set.Monad as Set

#ifdef ABSTRACT_ANALYSIS
query1 :: [(Attacker ExampleOne, Set (Parameter ExampleOne))]
query1 = [(attacker, accessibleParameters attacker) | attacker <- allValues ]
#endif

wellFormedExampleOne = fst $ (wellformed :: (Bool, Attacker ExampleOne))

query2 :: [(Attacker ExampleOne, Set (ResourceContainer ExampleOne,[Reason ExampleOne]))]
query2 = [(attacker, runWriterT $ containersFullyAccessibleBy attacker) | attacker <- allValues ]

query3 :: [(Attacker ExampleOne, Set (LinkingResource ExampleOne, [Reason ExampleOne]))]
query3 = [(attacker, runWriterT $ linksPayloadFullyAccessibleBy attacker) | attacker <- allValues ]

query4 :: [(Attacker ExampleOne, Set (LinkingResource ExampleOne, [Reason ExampleOne]))]
query4 = [(attacker, runWriterT $ linksMetaDataFullyAccessibleBy attacker) | attacker <- allValues ]

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