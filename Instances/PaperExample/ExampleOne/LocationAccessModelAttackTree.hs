{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.PaperExample.ExampleOne.LocationAccessModelAttackTree where
import Instances.PaperExample.ExampleOne.Palladio
import Instances.PaperExample.ExampleOne.Security

import LocationAccessModelAttackTree

import Data.Set.Monad as Set

import Misc

instance LocationAccessModelAttackTree ExampleOne where
    locationType UtilityRoom = Warehouse
    locationType LivingRoom  = Warehouse
    locationType Outdoors    = Unprotected

    locationsProtectedBy _                 = fromList allValues

    attackerCapabilities Guest             = fromList [Invited]
    attackerCapabilities Inhabitant        = fromList [Invited]
    attackerCapabilities PasserByAdversary = fromList []
