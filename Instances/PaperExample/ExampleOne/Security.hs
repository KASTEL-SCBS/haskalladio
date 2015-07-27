{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Instances.PaperExample.ExampleOne.Security where
import Data.Set.Monad as Set
import Instances.PaperExample.ExampleOne.Palladio
import Security
import Palladio
import Misc

import Data.Typeable


instance BasicDesignModel ExampleOne where
  data Attacker ExampleOne = Guest
                           | Inhabitant
                           | PasserByAdversary
                           deriving (Ord,Show,Eq,Bounded,Enum,Typeable)
  data DataSet ExampleOne = ConsumptionData
                          | BillingData
                          | PublicData
                          deriving (Ord,Show,Eq,Bounded,Enum,Typeable)

  datasets  = fromList allValues
  attackers = fromList allValues


  dataAllowedToBeAccessedBy Guest              = fromList [PublicData, ConsumptionData]
  dataAllowedToBeAccessedBy Inhabitant         = fromList [PublicData, ConsumptionData]
  dataAllowedToBeAccessedBy PasserByAdversary  = fromList [PublicData]


  classificationOf Value                               = fromList [ConsumptionData]
  classificationOf (Return GetValues)                  = fromList [ConsumptionData]
  classificationOf (Return DrawEnergyConsumptionGraph) = fromList [ConsumptionData]
  classificationOf (Return GetEnergyValue)             = fromList [ConsumptionData]
  classificationOf _                                   = fromList [PublicData]

  classificationOfCall _                               = fromList [PublicData]

instance ConcreteDesignModel ExampleOne where
   data TamperingAbility ExampleOne = Sealed
                                    | None
                                   deriving (Ord,Show,Eq, Typeable)
   data Location ExampleOne        = UtilityRoom
                                   | LivingRoom
                                   | Outdoors
                                   deriving (Ord,Show,Eq, Typeable)

   tamperingAbilities _    = fromList [None]

   unprotected = None

   locationsAccessibleBy Guest              = fromList [UtilityRoom, Outdoors, LivingRoom]
   locationsAccessibleBy Inhabitant         = fromList [UtilityRoom, Outdoors, LivingRoom]
   locationsAccessibleBy PasserByAdversary  = fromList [Outdoors]

   containerSecuredByMethod EnergyMeterRC          = fromList [Sealed]
   containerSecuredByMethod EnergyVisualizationRC  = fromList [None]

   furtherConnections EnergyMeterRC         = Complete
   furtherConnections EnergyVisualizationRC = Possible

   sharing EnergyMeterRC         = ControlledExclusive
   sharing EnergyVisualizationRC = OpenShared

   location EnergyMeterRC         = fromList [UtilityRoom]
   location EnergyVisualizationRC = fromList [LivingRoom]

   linkLocation link
     | link == wireless   = Set.fromList [LivingRoom,UtilityRoom,Outdoors]

