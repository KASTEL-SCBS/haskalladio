{-# LANGUAGE TypeFamilies #-} 
-- {-# LANGUAGE FlexibleContexts #-}
module PalladioInstance where

import qualified Palladio as P -- hiding (componentOf, runsOn, linkBetween)
import Data.Set

data ExampleOne = ExampleOne

type Id = Integer

digitalMeterContainer :: P.ResourceContainer ExampleOne 
digitalMeterContainer = Container { containerId = 1, name = "Digitalmeter" }


controllerContainer :: P.ResourceContainer ExampleOne 
controllerContainer = Container { containerId = 2, name = "Controller" }

digitalMeterContext = Context { contextId = 1, componentOf = DigitalMeter, runsOn = digitalMeterContainer }
controllerContext   = Context { contextId = 2, componentOf = Controller,   runsOn = controllerContainer }

link = Link { linkId = 1, linkBetween = (digitalMeterContainer, controllerContainer) }

instance  P.PalladioComponentModel ExampleOne where
  data Component ExampleOne = DigitalMeter | Controller              deriving (Ord,Eq,Show)
  data Interface ExampleOne = CurrentMeterDataReceiving              deriving (Ord,Eq,Show)
  data Parameter ExampleOne = Consumption                            deriving (Ord,Eq,Show)
  data Method ExampleOne    = StoreCurrentConsumption                deriving (Ord,Eq,Show)
  data DataType ExampleOne  = TimedConsumption                       deriving (Ord,Eq,Show)
  data AssemblyContext ExampleOne =
                              Context { contextId :: Id,
                                        componentOf :: P.Component ExampleOne,
                                        runsOn :: P.ResourceContainer ExampleOne
                                      } deriving (Ord,Eq,Show)
  data ResourceContainer ExampleOne =
                              Container { containerId :: Id,
                                          name :: String
                                        } deriving (Ord,Eq,Show)
  data LinkingResource ExampleOne =
                              Link { linkId :: Id,
                                     linkBetween :: (P.ResourceContainer ExampleOne, P.ResourceContainer ExampleOne)
                                   } deriving (Ord,Eq,Show)


  methods CurrentMeterDataReceiving = fromList [StoreCurrentConsumption]
  inputParameters StoreCurrentConsumption = fromList [Consumption]
  outputParameters StoreCurrentConsumption = fromList []
  
  typeOf Consumption = TimedConsumption
  
  components = fromList [DigitalMeter, Controller]

  provides DigitalMeter = fromList []
  provides Controller = fromList [CurrentMeterDataReceiving]

  requires DigitalMeter = fromList [CurrentMeterDataReceiving]
  requires Controller = fromList []
  
  isProvided _ = False
  isComplete _ = True
  
  isBasic _ = True
  
  subComponents _ = fromList []
  
  assembledTo _ _ _ = undefined

  delegatesProvides _ _ = undefined
  
  delegatesRequires  _ _ = undefined

  
  system = fromList [ digitalMeterContext, controllerContext ]
  
  systemProvides = fromList []
  systemRequires = fromList []

  systemAssembledTo context interface
   | context   == digitalMeterContext && 
     interface == CurrentMeterDataReceiving = P.ByAssembly controllerContext
   | otherwise                              = undefined

  resourcecontainers = fromList [digitalMeterContainer, controllerContainer]
  linkingresources = fromList [link]

  -- reuse record accessors
  runsOn = runsOn
  linkBetween = linkBetween
  componentOf = componentOf
