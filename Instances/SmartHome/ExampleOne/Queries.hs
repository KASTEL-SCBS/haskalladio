{-# LANGUAGE CPP #-}
module Instances.SmartHome.ExampleOne.Queries where

import Palladio
import Instances.SmartHome.ExampleOne.Palladio
import Security

#define ASTRACT_ANALYSIS
#ifdef ASTRACT_ANALYSIS
import AbstractAnalysis
#endif


import Instances.SmartHome.ExampleOne.Security
-- import Instances.SmartHome.ExampleOne.SimpleLinkModel
-- import Instances.SmartHome.ExampleOne.TamperableLinkModel
import Instances.SmartHome.ExampleOne.ComplexLinkModel


import Misc
import Data.Set.Monad

#ifdef ABSTRACT_ANALYSIS
query1 :: [(Attacker ExampleOne, Set (Parameter ExampleOne))]
query1 = [(attacker, accessibleParameters attacker) | attacker <- allValues ]
#endif

query2 :: [(Attacker ExampleOne, Set (ResourceContainer ExampleOne))]
query2 = [(attacker, containersFullyAccessibleBy attacker) | attacker <- allValues ]

query3 :: [(Attacker ExampleOne, Set (LinkingResource ExampleOne))]
query3 = [(attacker, linksPayloadFullyAccessibleBy attacker) | attacker <- allValues ]

query4 :: [(Attacker ExampleOne, Set (LinkingResource ExampleOne))]
query4 = [(attacker, linksMetaDataFullyAccessibleBy attacker) | attacker <- allValues ]

query5 :: [(Attacker ExampleOne, Set (DataSet ExampleOne))]
query5 = [(attacker, dataAccessibleTo attacker) | attacker <- allValues ]

pretty :: Show t => [t] -> IO ()
pretty list  = putStrLn $ showByLine $ list
