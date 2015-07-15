{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.PaperExample.ExampleOne.SimpleLinkModel where
import Instances.PaperExample.ExampleOne.Palladio
import Instances.PaperExample.ExampleOne.Security
import Security
import SimpleLinkModel
import Palladio
import Misc
import Data.Set as Set


instance SimpleEncryptionLinkAccessModel ExampleOne where
  isEncrypted link
     | link == wireless  = False
     | otherwise         = undefined

