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
                                   | None
                                   deriving (Ord,Show,Eq, Typeable)
   data Location ExampleOne        = Attended   -- "In der Wohnung!?!?"
                                   | Unattended -- "Im Keller?!?!?"
                                   | Outdoors
                                   | Public
                                   deriving (Ord,Show,Eq, Typeable, Enum, Bounded)

   unprotected = None
   
   tamperingAbilities Guest    = fromList [(location, None) | location <- allValues]
   tamperingAbilities HandyMan = fromList [(location, None) | location <- allValues]
   tamperingAbilities Anybody  = fromList [(location, ability) | location <- allValues,
                                                                 ability  <- [None, PlombeEntfernen, GerätÖffnen, WPA2Knacken]]
   tamperingAbilities Burglar  = fromList [(location, ability) | location <- allValues,
                                                                 ability  <- [None, PlombeEntfernen, GerätÖffnen, WPA2Knacken]]
   tamperingAbilities BlindDeafGuy = fromList [(location, None) | location <- allValues]


   containerSecuredByMethod DigitalMeterContainer = fromList [ (Unattended, PlombeEntfernen) ]
   containerSecuredByMethod TabletContainer       = fromList [ (Attended, GerätÖffnen) ]
   containerSecuredByMethod ControllerContainer   = fromList [ (Unattended, PlombeEntfernen) ]


   furtherConnections TabletContainer       = Possible
   furtherConnections ControllerContainer   = Possible
   furtherConnections DigitalMeterContainer = Possible

   sharing TabletContainer       = OpenShared
   sharing ControllerContainer   = ControlledExclusive
   sharing DigitalMeterContainer = ControlledExclusive

   location TabletContainer       = fromList [Attended]
   location ControllerContainer   = fromList [Unattended]
   location DigitalMeterContainer = fromList [Unattended]

   linkLocation link
     | link == linkMeterController   = Set.fromList [Unattended]
     | link == linkControllerTablet  = Set.fromList [Outdoors]

