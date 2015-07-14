{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.SmartHome.ExampleOne.TamperableLinkModel where

import Instances.SmartHome.ExampleOne.Palladio
import Instances.SmartHome.ExampleOne.Security
import Security
import TamperableLinkModel
import Palladio
import Misc
import Data.Set.Monad as Set



instance TamperAbilitiesLinkAccessModel ExampleOne where
   linkPayloadSecuredByMethod link
     | link == linkMeterController  = fromList [EthernetSnifferBesitzen]
     | link == linkControllerTablet = fromList [WPA2Knacken]

   linkMetaDataSecuredByMethod link
     | link == linkMeterController  = fromList []
     | link == linkControllerTablet = fromList [WPA2Knacken]

