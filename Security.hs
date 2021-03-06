{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Security where
import Data.Set.Monad as Set
import Palladio


import Data.Typeable
import Misc
import Reasons
import ReasonsModel

import Control.Monad.Trans.Class(lift)

data Insecure = Insecure deriving (Show,Eq, Ord)

{- Der Sicherheitsbegriff -}
class (BasicDesignModel m) => (SecurityProperty m) where
  isInSecureWithRespectTo ::  Attacker m -> WithReason m Insecure


{- So soll im einfachsten Fall ein Analyseresult eines Modells aussehen.
   * Mögliche Erweiterung: Berechnung eines "schwächsten Angreifers":
      weakestAttackerWithAccessTo :: DataSet m -> Attacker

   * Mögliche Erweiterung: "Beweis" der nachvollziehbar macht, wie und warum ein Angreifer Zugruff hat:
      dataAccessibleTo   :: Attacker m -> Set ((DataSet m, AccessProof m))

   * Mögliche Erweiterung: "kleinste Menge an Änderungen" die Notwendig sind, um das System abzusichern
      dataAccessibleTo   :: Attacker m -> Set ((DataSet m, ChangeRequest m))
-}
class (BasicDesignModel m) => AnalysisResult m where
  dataAccessibleTo   :: Attacker m -> WithReason m (DataSet m)

instance (AnalysisResult m, Reasons m) => (SecurityProperty m) where
  isInSecureWithRespectTo attacker = [ Insecure | accessed <- dataAccessibleTo attacker,
                                                         _ <- notDataAllowedToBeAccessedByM attacker accessed
                                     ]

--dataAccessibleToM :: (AnalysisResult m, Reasons m) => Attacker m -> WithReason m (DataSet m)
--dataAccessibleToM = liftR2 DataAccessibleTo dataAccessibleTo

dataAllowedToBeAccessedByM :: (AnalysisResult m, Reasons m) => Attacker m -> WithReason m (DataSet m)
dataAllowedToBeAccessedByM = liftA2 DataAllowedToBeAccessedBy dataAllowedToBeAccessedBy

notDataAllowedToBeAccessedByM :: (AnalysisResult m, Reasons m) => Attacker m -> DataSet m ->  WithReason m ()
notDataAllowedToBeAccessedByM = liftNot2 DataAllowedToBeAccessedBy dataAllowedToBeAccessedBy


class (BasicDesignModel m) => InterfaceUsage m where
  providedInterfacesDirectlyAccessibleTo :: Attacker m -> WithReason m (Interface m)
  requiredInterfacesDirectlyAccessibleTo :: Attacker m -> WithReason m (Interface m)



{- Egal wie genau nun ein Sicherheitsmodell aussieht (sharing,locations etc pp),
   es muss mindestens folgende Dinge spezifizieren:
-}
class (Ord (DataSet m), ReasonLike (DataSet m), ReasonLike (Attacker m),
       PalladioComponentModel m) => BasicDesignModel m where
  data Attacker m
  data DataSet m
  datasets  :: Set (DataSet m)
  attackers :: Set (Attacker m)

  dataAllowedToBeAccessedBy   :: Attacker m -> Set (DataSet m)

  classificationOf  :: (Parameter m) -> Set (DataSet m)
  classificationOfCall :: (Service m) -> Set (DataSet m)





{- Egal wie genau nun ein Sicherheitsmodell aussieht (sharing,locations etc pp),
   es muss möglich sein, damit Folgende Dinge herauszufinden:
-}
class (BasicDesignModel m) => AbstractDesignModel m where
  containersFullyAccessibleBy  :: Attacker m -> WithReason m (ResourceContainer m)
  linksDataAccessibleBy        :: Attacker m -> WithReason m (LinkingResource m, DataSet m)


{- Eine Variante eines konkreten Sicherheitsmodells -}
data Sharing = OpenShared
             | ControlledExclusive
             deriving (Show, Eq, Ord, Typeable)

data FurtherConnections = Possible
                        | Existing
                        | Complete
                        deriving (Show, Eq, Ord, Typeable)


class (Ord (Location m),
       BasicDesignModel m) => LinkAccessModel m where
  exposesPhsicallyAccessibleDataTo     :: LinkingResource m -> WithReason m (Attacker m, DataSet m)


class (BasicDesignModel m) => LocationAccessModel m where
  locationsAccessibleBy :: Attacker m -> WithReason m  (Location m)

class (Ord (Location m), ReasonLike (TamperingAbility m), ReasonLike (Location m),
       BasicDesignModel m) => ConcreteDesignModel m where
  data TamperingAbility m
  data Location m

  unprotected           :: TamperingAbility m
  tamperingAbilities    :: Attacker m ->  Set (Location m, TamperingAbility m)

  containerSecuredByMethod :: ResourceContainer m -> Set (Location m , TamperingAbility m)
  linkSecuredByMethod      :: LinkingResource   m -> Set (Location m , TamperingAbility m)

  furtherConnections :: ResourceContainer m -> FurtherConnections
  sharing            :: ResourceContainer m -> Sharing
  location           :: ResourceContainer m -> Set (Location m)

  linkLocation       :: LinkingResource m -> Set (Location m)


wellformed :: forall m. (ConcreteDesignModel m, InterfaceUsage m, Bounded (Attacker m), Enum (Attacker m), Bounded (Location m), Enum (Location m), Ord (TamperingAbility m), Enum (ResourceContainer m), Bounded (ResourceContainer m), All (LinkingResource m)) => (Bool, Attacker m)
wellformed = (
      (∀) (\(attacker  :: (Attacker m))          -> (∀) (\(location  :: (Location m))  ->  (location,unprotected) ∈ tamperingAbilities attacker))
  &&  (∀) (\(container :: (ResourceContainer m)) -> [location | (location, _)  <- containerSecuredByMethod container] ⊆ location container)
  &&  (∀) (\(container :: (ResourceContainer m)) -> (∀) (\(loc  :: (Location m))       -> (loc ∈ location container) → (

               (not $ isEmpty $ [method | (loc', method)  <- containerSecuredByMethod container, loc == loc'])
            && (      [method | (loc', method)  <- containerSecuredByMethod container, loc == loc'] == fromList [unprotected]
                || (not $ unprotected ∈ [method | (loc', method)  <- containerSecuredByMethod container, loc == loc'] )
               )
          )))
                                                      &&  (∀) (\(container :: (ResourceContainer m)) -> [location | (location, _)  <- containerSecuredByMethod container] ⊆ location container)
  &&  (∀∈) allValues' (\(link :: (LinkingResource m)) -> (∀) (\(loc  :: (Location m))       -> (loc ∈ linkLocation link) → (
               (not $ isEmpty $ [method | (loc', method)  <- linkSecuredByMethod link, loc == loc'])
            && (      [method | (loc', method)  <- linkSecuredByMethod link, loc == loc'] == fromList [unprotected]
                || (not $ unprotected ∈ [method | (loc', method)  <- linkSecuredByMethod link, loc == loc'] )
               )
          )))
 , undefined)

{- Damit erhält man ein Analyseergebnis folgendermaßen: -}
instance (ConcreteDesignModel m, LinkAccessModel m, Reasons m, LocationAccessModel m) => AbstractDesignModel m where
 containersFullyAccessibleBy attacker =
    [ container | (container, location) <- containersPhysicalAccessibleBy attacker,
                  (location',   method) <- containerSecuredByMethodM container,
                  (location'', ability) <- tamperingAbilitiesM attacker,
                  method == ability,
                  location == location',
                  location' == location''
    ] ⊔
    [ container | container <- lift $ resourcecontainers,
                  sharing            <- sharingM container,                       sharing == OpenShared,
                  furtherConnections <- furtherConnectionsM container, furtherConnections == Existing
    ] ⊔
    [ container | (container,_) <- containersPhysicalAccessibleBy attacker,
                  sharing            <- sharingM container,                       sharing == OpenShared,
                  furtherConnections <- furtherConnectionsM container, furtherConnections == Possible
    ] `hence` (Inferred2 ContainerFullyAccessibleBy attacker)

 linksDataAccessibleBy attacker =
    [ (link, dataset) | (link, location)      <- linksPhysicalAccessibleBy attacker,
                        (location',  method)  <- linkSecuredByMethodM link,
                        (location'', ability) <- tamperingAbilitiesM attacker,
                        method == ability,
                        location == location',
                        location' == location'',
                        (other, dataset) <- exposesPhsicallyAccessibleDataTo link,
                        attacker == other
    ] `hence` (Inferred2 LinksDataAccessibleBy attacker)



linksPhysicalAccessibleBy      :: (ConcreteDesignModel m, Reasons m, LocationAccessModel m) => Attacker m -> WithReason m (LinkingResource m, Location m)
linksPhysicalAccessibleBy attacker =
    [ (link, location) | link <- lift $ linkingresources,
             location <- linkLocationM link,
             accessible <- locationsAccessibleBy attacker,
             location == accessible
    ] `hence` (Inferred2 LinksPhysicallyAccessibleBy attacker)

containersPhysicalAccessibleBy :: (ConcreteDesignModel m, Reasons m, LocationAccessModel m) => Attacker m -> WithReason m (ResourceContainer m, Location m)
containersPhysicalAccessibleBy attacker =
    [ (container, location) | container <- lift $ resourcecontainers,
                  location <- locationM container,
                  accessible <- locationsAccessibleBy attacker,
                  location == accessible
    ] `hence` (Inferred2 ContainerPhysicallyAccessibleBy attacker)


{-
containersPhysicalAccessibleByM :: (ConcreteDesignModel m, Reasons m) => Attacker m -> WithReason m (ResourceContainer m)
containersPhysicalAccessibleByM = liftI2 ContainerPhysicallyAccessibleBy containersPhysicalAccessibleBy
-}

sharingM :: (ConcreteDesignModel m, Reasons m) => (ResourceContainer m) -> WithReason m Sharing
sharingM = liftF Sharing sharing

furtherConnectionsM :: (ConcreteDesignModel m, Reasons m) => (ResourceContainer m) -> WithReason m FurtherConnections
furtherConnectionsM = liftF FurtherConnections furtherConnections


tamperingAbilitiesM :: (ConcreteDesignModel m, Reasons m) => (Attacker m) -> WithReason m (Location m, TamperingAbility m)
tamperingAbilitiesM = liftA2 TamperingAbilities tamperingAbilities

containerSecuredByMethodM :: (ConcreteDesignModel m, Reasons m) => ResourceContainer m -> WithReason m (Location m, TamperingAbility m)
containerSecuredByMethodM = liftA2 ContainerSecuredByMethod  containerSecuredByMethod

linkSecuredByMethodM :: (ConcreteDesignModel m, Reasons m) => LinkingResource m -> WithReason m (Location m, TamperingAbility m)
linkSecuredByMethodM = liftA2 LinkSecuredByMethod  linkSecuredByMethod


linkLocationM :: (ConcreteDesignModel m, Reasons m) => (LinkingResource m) -> WithReason m (Location m)
linkLocationM = liftA2 LinkLocation linkLocation

locationM :: (ConcreteDesignModel m, Reasons m) => (ResourceContainer m) -> WithReason m (Location m)
locationM = liftA2 Location location

{-
interfacesAllowedToBeUsedBy :: Attacker m -> Set (Interface m)
dataAllowedToBeAccessedBy   :: Attacker m -> Set (DataSet m)
classificationOf  :: (Parameter m) -> Set (DataSet m)
classificationOfCall :: (Service m) -> Set (DataSet m)
-}
