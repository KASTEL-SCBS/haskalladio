{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module PalladioInstance where

import qualified Palladio as P -- hiding (componentOf, runsOn, linkBetween)
import Data.Set

data ExampleOne = ExampleOne

type Id = Integer


class All a where
  allValues' :: [a]

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

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
  data Component ExampleOne = DigitalMeter
                            | Controller
                            | Tablet
                            deriving (Ord,Eq,Show,Bounded,Enum)
  data Interface ExampleOne = CurrentMeterDataReceiving
                            | ConsumptionDataSending
                            | CurrentConsumptionDataDisplaying
                            deriving (Ord,Eq,Show,Bounded,Enum)
  data Parameter ExampleOne = Consumption
                            | Return (P.Method ExampleOne)
                            deriving (Ord,Eq,Show)
  data Method ExampleOne    = StoreCurrentConsumption
                            | GetCurrentConsumptionConsumptionDataSending
                            | GetHistoricConsumption
                            | GetCurrentConsumptionCurrentConsumptionDataDisplaying
                            deriving (Ord,Eq,Show,Bounded,Enum)
  data DataType ExampleOne  = TimedConsumption
                            | ConsumptionHistory
                            deriving (Ord,Eq,Show,Bounded,Enum)
  data AssemblyContext ExampleOne =
                              Context { contextId :: Id,
                                        componentOf :: P.Component ExampleOne,
                                        runsOn :: P.ResourceContainer ExampleOne
                                      } deriving (Ord,Eq,Show)
  data ResourceContainer ExampleOne = DigitalMeterContainer
                                    | ControllerContainer
                                    | TabletContainer
                                    deriving (Ord,Eq,Show,Bounded,Enum)
  data LinkingResource ExampleOne =
                              Link { linkId :: Id,
                                     linkBetween :: (P.ResourceContainer ExampleOne, P.ResourceContainer ExampleOne)
                                   } deriving (Ord,Eq,Show)


  methods CurrentMeterDataReceiving        = fromList [StoreCurrentConsumption]
  methods ConsumptionDataSending           = fromList [GetCurrentConsumptionConsumptionDataSending, GetHistoricConsumption]
  methods CurrentConsumptionDataDisplaying = fromList [GetCurrentConsumptionCurrentConsumptionDataDisplaying]
  
  inputParameters StoreCurrentConsumption = fromList [Consumption]
  inputParameters _                       = fromList []
  
  outputParameters StoreCurrentConsumption = fromList []
  outputParameters method = fromList [Return method]
  
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
