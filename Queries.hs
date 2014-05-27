module Queries where

import Palladio
import PalladioInstance
import Security
import SecurityInstance

import Misc
import Data.Set


query1 :: [(Attacker ExampleOne, Set (Parameter ExampleOne))]
query1 = [(attacker, accessibleParameters attacker) | attacker <- allValues ]

query2 :: [(Attacker ExampleOne, Set (ResourceContainer ExampleOne))]
query2 = [(attacker, containersFullyAccessibleBy attacker) | attacker <- allValues ]

query3 :: [(Attacker ExampleOne, Set (LinkingResource ExampleOne))]
query3 = [(attacker, linksFullyAccessibleBy attacker) | attacker <- allValues ]

pretty :: Show t => [t] -> IO ()
pretty list  = putStrLn $ showByLine $ list
