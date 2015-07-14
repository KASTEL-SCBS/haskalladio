{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.SmartHome.ExampleOne.ComplexLinkModel where
import Instances.SmartHome.ExampleOne.Palladio
import Instances.SmartHome.ExampleOne.Security

import Security
import ComplexLinkModel
import Palladio
import Misc
import Data.Set as Set


instance SimpleLinkProfileModel ExampleOne where
  decryptionAbilityOf  HandyMan     = NSA
  decryptionAbilityOf  Anybody      = Hacker
  decryptionAbilityOf  Burglar      = None
  decryptionAbilityOf  BlindDeafGuy = None

  hasPSK = HasWLANPSK
  linkType link
     | link == linkMeterController  = Ethernet
     | link == linkControllerTablet = WPA_PSK


