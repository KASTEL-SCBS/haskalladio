{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Instances.PaperExample.ExampleOne.Palladio where

import qualified Palladio as P -- hiding (componentOf, runsOn, linkBetween)
import Data.Set.Monad
import Misc
import Data.Typeable

data ExampleOne = ExampleOne deriving Typeable


type Id = Integer


instance P.ComponentRepository ExampleOne where
  data Component ExampleOne = DBMS
                            | EnergyVisualization
                            | EnergyMeter
                            deriving (Ord,Eq,Show,Bounded,Enum)
  data Interface ExampleOne = DatabaseInterface
                            | EnergyVisualizationI
                            | EnergyMeasurement
--                            | Billing
                            deriving (Ord,Eq,Show,Bounded,Enum, Typeable)
  data Parameter ExampleOne = Timestamp
                            | Value
                            | Start
                            | End
                            | Return (P.Service ExampleOne)
                            deriving (Ord,Eq,Show,Typeable)
  data Service ExampleOne   = GetValues
                            | StoreValue
                            | DrawEnergyConsumptionGraph
                            | GetEnergyValue
                            | GetCustomerCredentials
                            | GetHighestValue
                            deriving (Ord,Eq,Show,Bounded,Enum,Typeable)
  data DataType ExampleOne  = IntT
                            | ImageT
                            | IntArrayT
                            deriving (Ord,Eq,Show,Bounded,Enum)


  services DatabaseInterface                  = fromList [GetValues, StoreValue]
  services EnergyVisualizationI               = fromList [DrawEnergyConsumptionGraph]
  services EnergyMeasurement                  = fromList [GetEnergyValue, GetCustomerCredentials]
--  services Billing                            = fromList [GetCustomerCredentials]
  
  inputParameters StoreValue                  = fromList [Timestamp, Value]
  inputParameters GetValues                   = fromList [Start, End]
  inputParameters DrawEnergyConsumptionGraph  = fromList []
  inputParameters GetEnergyValue              = fromList []
  inputParameters GetCustomerCredentials      = fromList []

  outputParameters StoreValue                 = fromList []
  outputParameters service                    = fromList [Return service]
  
  typeOf Timestamp = IntT
  typeOf Value     = IntT
  typeOf Start     = IntT
  typeOf End       = IntT
  typeOf (Return GetValues)                  = IntArrayT
  typeOf (Return DrawEnergyConsumptionGraph) = ImageT
  typeOf (Return GetHighestValue)            = IntT
  typeOf (Return GetEnergyValue)             = IntT
  typeOf (Return GetCustomerCredentials)     = IntT
  typeOf (Return StoreValue)                 = error "no Type for non-Existing Parameter"
  
  components = fromList allValues

  provides DBMS                = fromList [DatabaseInterface]
  provides EnergyVisualization = fromList [EnergyVisualizationI]
  provides EnergyMeter         = fromList [EnergyMeasurement]

  requires DBMS                = fromList []
  requires EnergyVisualization = fromList [DatabaseInterface, EnergyMeasurement]
  requires EnergyMeter         = fromList []

  
  isProvided _ = False
  isComplete _ = True
  
  isBasic _ = True
  
  subComponents _ = fromList []
  
  assembledTo _ _ _ = undefined

  delegatesProvides _ _ = undefined
  
  delegatesRequires  _ _ = undefined




database                     = Context { contextId = 1, componentOf = DBMS,                runsOn = EnergyVisualizationRC }
energyMeterAssemblyContext   = Context { contextId = 2, componentOf = EnergyMeter,         runsOn = EnergyMeterRC }
energyVisualization          = Context { contextId = 3, componentOf = EnergyVisualization, runsOn = EnergyVisualizationRC }
instance All (P.AssemblyContext ExampleOne) where
  allValues' = [database, energyMeterAssemblyContext, energyVisualization]


wireless  = Link { linkId = 1, linkBetween = (EnergyVisualizationRC, EnergyMeterRC) }
instance All (P.LinkingResource ExampleOne) where
  allValues' = [wireless]


instance  P.PalladioComponentModel ExampleOne where
  data AssemblyContext ExampleOne =
                              Context { contextId :: Id,
                                        componentOf :: P.Component ExampleOne,
                                        runsOn :: P.ResourceContainer ExampleOne
                                      } deriving (Ord,Eq,Show)
  data ResourceContainer ExampleOne = EnergyVisualizationRC
                                    | EnergyMeterRC
                                    deriving (Ord,Eq,Show,Bounded,Enum,Typeable)
  data LinkingResource ExampleOne =
                              Link { linkId :: Id,
                                     linkBetween :: (P.ResourceContainer ExampleOne, P.ResourceContainer ExampleOne)
                                   } deriving (Ord,Eq,Show, Typeable)





  system = fromList allValues'

  systemProvides = fromList [EnergyVisualizationI, EnergyMeasurement]
  systemRequires = fromList []

  systemAssembledTo context interface
   | context   == energyVisualization  &&
     interface == DatabaseInterface = P.ByAssembly database
   | context   == energyVisualization  &&
     interface == EnergyMeasurement = P.ByAssembly energyMeterAssemblyContext 
   | otherwise                         = error $ "unknown Context: " ++ (show context)

  systemProvidesAsssembledTo EnergyVisualizationI = energyVisualization
  systemProvidesAsssembledTo EnergyMeasurement    = energyMeterAssemblyContext
  systemProvidesAsssembledTo _                    = undefined

  resourcecontainers = fromList allValues
  linkingresources = fromList allValues'

  -- reuse record accessors
  runsOn = runsOn
  linkBetween = linkBetween
  componentOf = componentOf
