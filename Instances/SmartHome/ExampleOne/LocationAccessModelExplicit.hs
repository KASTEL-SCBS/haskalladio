{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.SmartHome.ExampleOne.LocationAccessModelExplicit where
import Instances.SmartHome.ExampleOne.Palladio
import Instances.SmartHome.ExampleOne.Security

import LocationAccessModelExplicit

import Data.Set.Monad as Set


instance LocationAccessModelExplicit ExampleOne where
   locationsAccessibleByExplicit Guest    = fromList [Outdoors, Public, Attended]
   locationsAccessibleByExplicit HandyMan = fromList [Outdoors, Public, Unattended]
   locationsAccessibleByExplicit Anybody  = fromList [Outdoors, Public]
   locationsAccessibleByExplicit Burglar  = fromList [Outdoors, Public, Unattended]
   locationsAccessibleByExplicit BlindDeafGuy = fromList [Outdoors, Public]
