{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.SmartHome.ExampleOne.TamperableLinkModel where

import Instances.SmartHome.ExampleOne.Palladio
import Instances.SmartHome.ExampleOne.Security
import Security
import TamperableLinkModel
import Palladio
import Misc
import Data.Set as Set



instance TamperAbilitiesLinkAccessModel ExampleOne where
   linkTamperableByAttackerWithAbilities link abilities
     | link == linkMeterController  = EthernetSnifferBesitzen ∈ abilities
     | link == linkControllerTablet = WPA2Knacken ∈ abilities

