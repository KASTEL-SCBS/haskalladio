{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.PaperExample.ExampleOne.LocationAccessModelExplicit where
import Instances.PaperExample.ExampleOne.Palladio
import Instances.PaperExample.ExampleOne.Security

import LocationAccessModelExplicit

import Data.Set.Monad as Set


instance LocationAccessModelExplicit ExampleOne where
   locationsAccessibleByExplicit Guest              = fromList [Outdoors, LivingRoom]
   locationsAccessibleByExplicit Inhabitant         = fromList [UtilityRoom, Outdoors, LivingRoom]
   locationsAccessibleByExplicit PasserByAdversary  = fromList [Outdoors]
