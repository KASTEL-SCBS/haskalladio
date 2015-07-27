{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.PaperExample.ExampleOne.InterfaceUsageExplicit where
import Instances.PaperExample.ExampleOne.Palladio
import Instances.PaperExample.ExampleOne.Security

import Security
import InterfaceUsageExplicit

import Data.Set.Monad as Set


instance InterfaceUsageExplicit ExampleOne where
  providedInterfacesAllowedToBeUsedBy Guest              = fromList []
  providedInterfacesAllowedToBeUsedBy Inhabitant         = fromList [EnergyVisualizationI]
  providedInterfacesAllowedToBeUsedBy PasserByAdversary  = fromList []

  requiredInterfacesAllowedToBeUsedBy _ = fromList []

