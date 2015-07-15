{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.PaperExample.ExampleOne.SimpleLinkModelWithExceptions where
import Instances.PaperExample.ExampleOne.Palladio
import Instances.PaperExample.ExampleOne.Security
import Security
import SimpleLinkModelWithExceptions
import Palladio
import Misc
import Data.Set.Monad as Set


instance SimpleEncryptionWithExceptionsLinkAccessModel ExampleOne where
  isEncryptedButFor link
     | link == wireless  = fromList [PublicData]
     | otherwise         = undefined

