{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instances.SmartHome.SecurityInstance where
import Data.Set as Set
import Instances.SmartHome.PalladioInstance
import Security
import Palladio
import Misc

instance BasicDesignModel ExampleOne where
  data Attacker ExampleOne = Guest
                           | HandyMan
                           | Anybody
                           | Burglar
                           | BlindDeafGuy
                           deriving (Ord,Show,Eq,Bounded,Enum)
  data DataSet ExampleOne = InhabitantData
                          | ProviderData
                          | PublicData
                          deriving (Ord,Show,Eq,Bounded,Enum)

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
  
  classificationOf Consumption = InhabitantData
  classificationOf (Return GetCurrentConsumptionConsumptionDataSending) = InhabitantData
  classificationOf (Return GetCurrentConsumptionCurrentConsumptionDataDisplaying) = InhabitantData
  classificationOf (Return GetHistoricConsumption) = ProviderData
  
  classificationOfCall _ = PublicData


instance ConcreteDesignModel ExampleOne where
   data TamperingMethod ExampleOne = PlombeEntfernen
                                   | GerätÖffnen
                                   | WPA2Knacken
                                   | EthernetSnifferBesitzen
                                   deriving (Ord,Show,Eq)
   data Location ExampleOne        = Attended   -- "In der Wohnung!?!?"
                                   | Unattended -- "Im Keller?!?!?"
                                   | Outdoors
                                   | Public
                                   deriving (Ord,Show,Eq)

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

   containerTamperableByAttackerWithAbilities DigitalMeterContainer abilities =
      PlombeEntfernen ∈ abilities &&
      GerätÖffnen ∈ abilities
   containerTamperableByAttackerWithAbilities TabletContainer       abilities =
      GerätÖffnen ∈ abilities
   containerTamperableByAttackerWithAbilities ControllerContainer   abilities =
      PlombeEntfernen ∈ abilities
   
   linkTamperableByAttackerWithAbilities link abilities
     | link == linkMeterController  = EthernetSnifferBesitzen ∈ abilities
     | link == linkControllerTablet = WPA2Knacken ∈ abilities

   furtherConnections TabletContainer       = Possible
   furtherConnections ControllerContainer   = Possible
   furtherConnections DigitalMeterContainer = Possible

   sharing TabletContainer       = OpenShared
   sharing ControllerContainer   = ControlledExclusive
   sharing DigitalMeterContainer = ControlledExclusive

   locality TabletContainer       = Attended
   locality ControllerContainer   = Unattended
   locality DigitalMeterContainer = Unattended

   linkLocality link
     | link == linkMeterController   = Unattended
     | link == linkControllerTablet  = Outdoors

