{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Instances.SmartHome.ExampleOne.InterfaceUsageByGroup where
import Instances.SmartHome.ExampleOne.Palladio
import Instances.SmartHome.ExampleOne.Security

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


  isInDomain CurrentMeterDataReceiving        =  Energy
  isInDomain ConsumptionDataSending           =  Energy
  isInDomain CurrentConsumptionDataDisplaying =  Energy

  hasAccessToDomains Inhabitants     = fromList allValues
  hasAccessToDomains Administrators  = fromList allValues

  isMemberOfGroups Guest           = fromList []
  isMemberOfGroups HandyMan        = fromList [Inhabitants]
  isMemberOfGroups Anybody         = fromList [Inhabitants]
  isMemberOfGroups BlindDeafGuy    = fromList []
  isMemberOfGroups Burglar         = fromList []

