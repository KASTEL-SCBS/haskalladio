{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Instances.SmartHome.ExampleOne.Palladio where

import qualified Palladio as P -- hiding (componentOf, runsOn, linkBetween)
import Data.Set.Monad 
import Misc
import Data.Typeable

data ExampleOne = ExampleOne deriving Typeable


type Id = Integer


instance P.ComponentRepository ExampleOne where
  data Component ExampleOne = DigitalMeter
                            | Controller
                            | Tablet
                            deriving (Ord,Eq,Show,Bounded,Enum)
  data Interface ExampleOne = CurrentMeterDataReceiving
                            | ConsumptionDataSending
                            | CurrentConsumptionDataDisplaying
                            deriving (Ord,Eq,Show,Bounded,Enum, Typeable)
  data Parameter ExampleOne = Consumption
                            | Return (P.Service ExampleOne)
                            deriving (Ord,Eq,Show,Typeable)
  data Service ExampleOne    = StoreCurrentConsumption
                            | GetCurrentConsumptionConsumptionDataSending
                            | GetHistoricConsumption
                            | GetCurrentConsumptionCurrentConsumptionDataDisplaying
                            deriving (Ord,Eq,Show,Bounded,Enum,Typeable)
  data DataType ExampleOne  = TimedConsumption
                            | ConsumptionHistory
                            deriving (Ord,Eq,Show,Bounded,Enum)


  services CurrentMeterDataReceiving        = fromList [StoreCurrentConsumption]
  services ConsumptionDataSending           = fromList [GetCurrentConsumptionConsumptionDataSending, GetHistoricConsumption]
  services CurrentConsumptionDataDisplaying = fromList [GetCurrentConsumptionCurrentConsumptionDataDisplaying]
  
  inputParameters StoreCurrentConsumption = fromList [Consumption]
  inputParameters _                       = fromList []
  
  outputParameters StoreCurrentConsumption = fromList []
  outputParameters service = fromList [Return service]
  
  typeOf Consumption = TimedConsumption
  typeOf (Return GetCurrentConsumptionConsumptionDataSending)           = TimedConsumption
  typeOf (Return GetCurrentConsumptionCurrentConsumptionDataDisplaying) = TimedConsumption
  typeOf (Return GetHistoricConsumption) = ConsumptionHistory
  
  components = fromList allValues

  provides DigitalMeter = fromList []
  provides Controller = fromList [CurrentMeterDataReceiving, ConsumptionDataSending]
  provides Tablet = fromList [CurrentConsumptionDataDisplaying]
  
  requires DigitalMeter = fromList [CurrentMeterDataReceiving]
  requires Controller = fromList []
  requires Tablet     = fromList [ConsumptionDataSending]
  
  isProvided _ = False
  isComplete _ = True
  
  isBasic _ = True
  
  subComponents _ = fromList []
  
  assembledTo _ _ _ = undefined

  delegatesProvides _ _ = undefined
  
  delegatesRequires  _ _ = undefined







digitalMeterContext = Context { contextId = 1, componentOf = DigitalMeter, runsOn = DigitalMeterContainer }
controllerContext   = Context { contextId = 2, componentOf = Controller,   runsOn = ControllerContainer }
tabletContext       = Context { contextId = 3, componentOf = Tablet, runsOn = TabletContainer }
--meterReaderrContext = Context { contextId = 4, componentOf = Controller,   runsOn = controllerContainer }
instance All (P.AssemblyContext ExampleOne) where
  allValues' = [digitalMeterContext, controllerContext, tabletContext]


linkMeterController  = Link { linkId = 1, linkBetween = (DigitalMeterContainer, ControllerContainer) }
linkControllerTablet = Link { linkId = 2, linkBetween = (ControllerContainer, TabletContainer) }
instance All (P.LinkingResource ExampleOne) where
  allValues' = [linkMeterController, linkControllerTablet]


instance  P.PalladioComponentModel ExampleOne where
  data AssemblyContext ExampleOne =
                              Context { contextId :: Id,
                                        componentOf :: P.Component ExampleOne,
                                        runsOn :: P.ResourceContainer ExampleOne
                                      } deriving (Ord,Eq,Show)
  data ResourceContainer ExampleOne = DigitalMeterContainer
                                    | ControllerContainer
                                    | TabletContainer
                                    deriving (Ord,Eq,Show,Bounded,Enum,Typeable)
  data LinkingResource ExampleOne =
                              Link { linkId :: Id,
                                     linkBetween :: (P.ResourceContainer ExampleOne, P.ResourceContainer ExampleOne)
                                   } deriving (Ord,Eq,Show, Typeable)





  system = fromList [ digitalMeterContext, controllerContext, tabletContext ]

  systemProvides = fromList [CurrentConsumptionDataDisplaying]
  systemRequires = fromList []

  systemAssembledTo context interface
   | context   == digitalMeterContext &&
     interface == CurrentMeterDataReceiving = P.ByAssembly controllerContext
   | context   == tabletContext &&
     interface == ConsumptionDataSending    = P.ByAssembly controllerContext
   | otherwise                              = undefined

  systemProvidesAsssembledTo CurrentConsumptionDataDisplaying = tabletContext
  systemProvidesAsssembledTo _ = undefined

  resourcecontainers = fromList allValues
  linkingresources = fromList allValues'

  -- reuse record accessors
  runsOn = runsOn
  linkBetween = linkBetween
  componentOf = componentOf
