{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE MonadComprehensions #-}

module LocationAccessModelAttackTree where
import Data.Set.Monad as Set
import Palladio
import Security

import Control.Monad.Trans.Class(lift)

import Misc
import Reasons
import ReasonsModel


data AttackerCapability = ClimbOverFence
                        | UseCarpetOnBarbs
                        | WearProtectiveClothing
                        | EnterThroughGate
                        | EnterThroughDoor
                        | EnterThroughLoadingDock
                        | LaserCameras
                        | VideoLoopCameras
                        | Invited
                        deriving (Eq, Ord, Show, Enum, Bounded)
data DefenseMeasure = BarbedWire
                    | MonitorWithBioMetricSensors
                    | EmployGuards
                    deriving (Eq, Ord, Show, Enum, Bounded)

data LocationType = Warehouse
                  | Unprotected

class (Ord (Interface m),
       ConcreteDesignModel m) => LocationAccessModelAttackTree m where

  locationType         :: Location m -> LocationType
  locationsProtectedBy :: Location m -> Set DefenseMeasure
  attackerCapabilities :: Attacker m -> Set AttackerCapability


evalTree Warehouse attacker location
                           = (Invited ∈ capabilities)  || breakAndEntering
  where breakAndEntering   = getOntoPremises && getIntoWarehouse && (MonitorWithBioMetricSensors ∈ measures → disableCameras)
        getOntoPremises    = climbOverFence || (EnterThroughGate ∈ capabilities)
        getIntoWarehouse   = enterThroughDoor || (EnterThroughLoadingDock ∈ capabilities)
        enterThroughDoor   = (EnterThroughDoor ∈ capabilities &&
                              MonitorWithBioMetricSensors ∈ measures → False)
        climbOverFence     =  ClimbOverFence ∈ capabilities &&
                              (BarbedWire ∈ measures → guardAgainstBarbs)
        guardAgainstBarbs  = (UseCarpetOnBarbs  ∈ capabilities) || (WearProtectiveClothing ∈ capabilities)
        disableCameras     = (LaserCameras ∈ capabilities) || videoLoopCameras
        videoLoopCameras   = VideoLoopCameras ∈ capabilities &&
                             (EmployGuards ∈ measures → False)

        measures     = locationsProtectedBy location
        capabilities = attackerCapabilities attacker

evalTree Unprotected _ _ = True

instance (BasicDesignModel m, LocationAccessModelAttackTree m, Reasons m, Enum (Location m), Bounded (Location m)) => LocationAccessModel m where
  locationsAccessibleBy = liftA2 LocationsAccessibleBy (\attacker -> fromList [ location | location <- allValues,
                                                                                           evalTree (locationType location) attacker location])

