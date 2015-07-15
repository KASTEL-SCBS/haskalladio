{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.PaperExample.ExampleOne.ComplexLinkModel where
import Instances.PaperExample.ExampleOne.Palladio
import Instances.PaperExample.ExampleOne.Security

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


