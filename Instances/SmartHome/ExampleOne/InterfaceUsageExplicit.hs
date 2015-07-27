{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.SmartHome.ExampleOne.InterfaceUsageExplicit where
import Instances.SmartHome.ExampleOne.Palladio
import Instances.SmartHome.ExampleOne.Security

import Security
import InterfaceUsageExplicit

import Data.Set.Monad as Set


instance InterfaceUsageExplicit ExampleOne where
  providedInterfacesAllowedToBeUsedBy Guest    = fromList [CurrentConsumptionDataDisplaying]
  providedInterfacesAllowedToBeUsedBy HandyMan = fromList []
  providedInterfacesAllowedToBeUsedBy Anybody  = fromList []
  providedInterfacesAllowedToBeUsedBy Burglar  = fromList []
  providedInterfacesAllowedToBeUsedBy BlindDeafGuy = fromList []

  requiredInterfacesAllowedToBeUsedBy _ = fromList []
