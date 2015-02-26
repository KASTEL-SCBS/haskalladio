{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Instances.SmartHome.ExampleOne.Security where
import Data.Set.Monad as Set
import Instances.SmartHome.ExampleOne.Palladio
import Security
import Palladio
import Misc

import Data.Typeable


instance BasicDesignModel ExampleOne where
  data Attacker ExampleOne = Guest
                           | HandyMan
                           | Anybody
                           | Burglar
                           | BlindDeafGuy
                           deriving (Ord,Show,Eq,Bounded,Enum,Typeable)
  data DataSet ExampleOne = InhabitantData
                          | ProviderData
                          | PublicData
                          deriving (Ord,Show,Eq,Bounded,Enum,Typeable)

  datasets = fromList [InhabitantData, ProviderData, PublicData]
  attackers = fromList [Guest, HandyMan, Anybody]

  interfacesAllowedToBeUsedBy Guest    = fromList [CurrentConsumptionDataDisplaying]
  interfacesAllowedToBeUsedBy HandyMan = fromList []
  interfacesAllowedToBeUsedBy Anybody  = fromList []
  interfacesAllowedToBeUsedBy Burglar  = fromList []
  interfacesAllowedToBeUsedBy BlindDeafGuy = fromList []

  dataAllowedToBeAccessedBy Guest    = fromList [PublicData, InhabitantData]
  dataAllowedToBeAccessedBy HandyMan = fromList [PublicData, ProviderData]
  dataAllowedToBeAccessedBy Anybody  = fromList [PublicData]
  dataAllowedToBeAccessedBy Burglar  = fromList [PublicData]
  dataAllowedToBeAccessedBy BlindDeafGuy = fromList [PublicData]
  
  classificationOf Consumption = fromList [InhabitantData]
  classificationOf (Return GetCurrentConsumptionConsumptionDataSending) = fromList [InhabitantData]
  classificationOf (Return GetCurrentConsumptionCurrentConsumptionDataDisplaying) = fromList [InhabitantData]
  classificationOf (Return GetHistoricConsumption) = fromList [ProviderData]
  
  classificationOfCall _ = fromList [PublicData]

instance ConcreteDesignModel ExampleOne where
   data TamperingAbility ExampleOne = PlombeEntfernen
                                   | GerätÖffnen
                                   | WPA2Knacken
                                   | EthernetSnifferBesitzen
                                   | HasWLANPSK
                                   deriving (Ord,Show,Eq, Typeable)
   data Location ExampleOne        = Attended   -- "In der Wohnung!?!?"
                                   | Unattended -- "Im Keller?!?!?"
                                   | Outdoors
                                   | Public
                                   deriving (Ord,Show,Eq, Typeable)

   tamperingAbilities Guest    = fromList []
   tamperingAbilities HandyMan = fromList []
   tamperingAbilities Anybody  = fromList [PlombeEntfernen, GerätÖffnen, WPA2Knacken]
   tamperingAbilities Burglar  = fromList [PlombeEntfernen, GerätÖffnen, WPA2Knacken]
   tamperingAbilities BlindDeafGuy = fromList []


   locationsAccessibleBy Guest    = fromList [Outdoors, Public, Attended]
   locationsAccessibleBy HandyMan = fromList [Outdoors, Public, Unattended]
   locationsAccessibleBy Anybody  = fromList [Outdoors, Public]
   locationsAccessibleBy Burglar  = fromList [Outdoors, Public, Unattended]
   locationsAccessibleBy BlindDeafGuy = fromList [Outdoors, Public]


   containerSecuredByMethod DigitalMeterContainer = fromList [ PlombeEntfernen ]
   containerSecuredByMethod TabletContainer       = fromList [ GerätÖffnen ]
   containerSecuredByMethod ControllerContainer   = fromList [ PlombeEntfernen ]


   furtherConnections TabletContainer       = Possible
   furtherConnections ControllerContainer   = Possible
   furtherConnections DigitalMeterContainer = Possible

   sharing TabletContainer       = OpenShared
   sharing ControllerContainer   = ControlledExclusive
   sharing DigitalMeterContainer = ControlledExclusive

   location TabletContainer       = Attended
   location ControllerContainer   = Unattended
   location DigitalMeterContainer = Unattended

   linkLocation link
     | link == linkMeterController   = Set.fromList [Unattended]
     | link == linkControllerTablet  = Set.fromList [Outdoors]

