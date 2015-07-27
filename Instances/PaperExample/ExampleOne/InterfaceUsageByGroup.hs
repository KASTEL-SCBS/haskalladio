{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Instances.PaperExample.ExampleOne.InterfaceUsageByGroup where
import Instances.PaperExample.ExampleOne.Palladio
import Instances.PaperExample.ExampleOne.Security

import Security
import InterfaceUsageByGroup
import Misc

import Data.Set.Monad as Set
import Data.Typeable



instance InterfaceUsageByGroup ExampleOne where
  data AccessControlDomain ExampleOne = Energy
                                      deriving (Ord,Show,Eq,Bounded,Enum,Typeable)

  data UserGroup ExampleOne  = Inhabitants
                             | Administrators
                             deriving (Ord,Show,Eq,Bounded,Enum,Typeable)


  isInDomain DatabaseInterface     =  Energy
  isInDomain EnergyVisualizationI  =  Energy
  isInDomain EnergyMeasurement     =  Energy

  hasAccessToDomains Inhabitants     = fromList allValues
  hasAccessToDomains Administrators  = fromList allValues

  isMemberOfGroups Guest             = fromList []
  isMemberOfGroups Inhabitant        = fromList [Inhabitants]
  isMemberOfGroups PasserByAdversary = fromList []
