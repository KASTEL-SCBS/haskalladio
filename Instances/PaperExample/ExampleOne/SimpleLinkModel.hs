{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.SmartHome.ExampleOne.SimpleLinkModel where
import Instances.SmartHome.ExampleOne.Palladio
import Instances.SmartHome.ExampleOne.Security
import Security
import SimpleLinkModel
import Palladio
import Misc
import Data.Set as Set


instance SimpleEncryptionLinkAccessModel ExampleOne where
  isEncrypted link
     | link == linkMeterController  = False
     | link == linkControllerTablet = True


