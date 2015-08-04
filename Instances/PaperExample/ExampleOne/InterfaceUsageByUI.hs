{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Instances.PaperExample.ExampleOne.InterfaceUsageByUI where
import Instances.PaperExample.ExampleOne.Palladio
import Instances.PaperExample.ExampleOne.Security

import Security
import InterfaceUsageByUI
import Misc

import Data.Set.Monad as Set
import Data.Typeable



instance InterfaceUsageByUI ExampleOne where
  uiInterfaceOn EnergyMeterRC         = fromList []
  uiInterfaceOn EnergyVisualizationRC = fromList [EnergyVisualizationI]

