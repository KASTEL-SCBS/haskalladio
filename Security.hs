{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}


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
                                                         _ <- because [Inferred2 DataAccessibleTo attacker accessed],
                                                         _ <- notDataAllowedToBeAccessedByM attacker accessed
                                     ]

--dataAccessibleToM :: (AnalysisResult m, Reasons m) => Attacker m -> WithReason m (DataSet m)
--dataAccessibleToM = liftR2 DataAccessibleTo dataAccessibleTo

dataAllowedToBeAccessedByM :: (AnalysisResult m, Reasons m) => Attacker m -> WithReason m (DataSet m)
dataAllowedToBeAccessedByM = liftA2 DataAllowedToBeAccessedBy dataAllowedToBeAccessedBy

notDataAllowedToBeAccessedByM :: (AnalysisResult m, Reasons m) => Attacker m -> DataSet m ->  WithReason m ()
notDataAllowedToBeAccessedByM = liftNot2 DataAllowedToBeAccessedBy dataAllowedToBeAccessedBy

{- Egal wie genau nun ein Sicherheitsmodell aussieht (sharing,locations etc pp),
   es muss mindestens folgende Dinge spezifizieren:
-}
class (Ord (DataSet m), ReasonLike (DataSet m), ReasonLike (Attacker m),
       PalladioComponentModel m) => BasicDesignModel m where
  data Attacker m
  data DataSet m
  datasets  :: Set (DataSet m)
  attackers :: Set (Attacker m)

  interfacesAllowedToBeUsedBy :: Attacker m -> Set (Interface m)
  dataAllowedToBeAccessedBy   :: Attacker m -> Set (DataSet m)

  classificationOf  :: (Parameter m) -> Set (DataSet m)
  classificationOfCall :: (Service m) -> Set (DataSet m)





{- Egal wie genau nun ein Sicherheitsmodell aussieht (sharing,locations etc pp),
   es muss möglich sein, damit Folgende Dinge herauszufinden:
-}
class (BasicDesignModel m) => AbstractDesignModel m where
  containersFullyAccessibleBy  :: Attacker m -> WithReason m (ResourceContainer m)
  linksMetaDataFullyAccessibleBy       :: Attacker m -> Set (LinkingResource m)
  linksPayloadFullyAccessibleBy       :: Attacker m -> Set (LinkingResource m)







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
  -- linksExposingPayloadWhenPysicalliAccessibleTo
  exposesPhsicallyAccessiblePayloadTo  :: LinkingResource m -> Attacker m ->  Bool
  exposesPhsicallyAccessibleMetaDataTo :: LinkingResource m -> Attacker m ->  Bool


class (Ord (Location m), ReasonLike (TamperingAbility m),
       BasicDesignModel m) => ConcreteDesignModel m where
  data TamperingAbility m
  data Location m

  tamperingAbilities    :: Attacker m -> Set (TamperingAbility m)

  locationsAccessibleBy :: Attacker m -> Set (Location m)

  containerSecuredByMethod :: ResourceContainer m -> Set (TamperingAbility m)

  furtherConnections :: ResourceContainer m -> FurtherConnections
  sharing            :: ResourceContainer m -> Sharing
  location           :: ResourceContainer m -> Location m

  linkLocation       :: LinkingResource m -> Set (Location m)


{- Damit erhält man ein Analyseergebnis folgendermaßen: -}
instance (ConcreteDesignModel m, LinkAccessModel m, Reasons m) => AbstractDesignModel m where
 containersFullyAccessibleBy attacker =
    [ container | container <- containersPhysicalAccessibleByM attacker,
                  method    <- containerSecuredByMethodM container,
                  ability   <- tamperingAbilitiesM attacker,
                  method == ability
    ] ⊔
    [ container | container <- lift $ resourcecontainers,
                  sharing            <- sharingM container,                       sharing == OpenShared,
                  furtherConnections <- furtherConnectionsM container, furtherConnections == Existing
    ] ⊔
    [ container | container <- lift $ containersPhysicalAccessibleBy attacker,
                  sharing            <- sharingM container,                       sharing == OpenShared,
                  furtherConnections <- furtherConnectionsM container, furtherConnections == Possible
    ]

 linksPayloadFullyAccessibleBy attacker =
    [ link | link <- linksPhysicalAccessibleBy attacker,
             link `exposesPhsicallyAccessiblePayloadTo` attacker
    ]

 linksMetaDataFullyAccessibleBy attacker =
    [ link | link <- linksPhysicalAccessibleBy attacker,
             link `exposesPhsicallyAccessibleMetaDataTo` attacker
    ]



linksPhysicalAccessibleBy      :: (ConcreteDesignModel m ) => Attacker m -> Set (LinkingResource m)
linksPhysicalAccessibleBy attacker =
    [ link | link <- linkingresources,
             not $ isEmpty (linkLocation link ∩ locationsAccessibleBy attacker)
    ]

containersPhysicalAccessibleBy :: (ConcreteDesignModel m ) => Attacker m -> Set (ResourceContainer m)
containersPhysicalAccessibleBy attacker =
    [ container | container <- resourcecontainers,
                  location container ∈ locationsAccessibleBy attacker
    ]


containersPhysicalAccessibleByM :: (ConcreteDesignModel m, Reasons m) => Attacker m -> WithReason m (ResourceContainer m)
containersPhysicalAccessibleByM = liftI2 ContainerPhysicallyAccessibleBy containersPhysicalAccessibleBy

sharingM :: (ConcreteDesignModel m, Reasons m) => (ResourceContainer m) -> WithReason m Sharing
sharingM = liftF Sharing sharing

furtherConnectionsM :: (ConcreteDesignModel m, Reasons m) => (ResourceContainer m) -> WithReason m FurtherConnections
furtherConnectionsM = liftF FurtherConnections furtherConnections


tamperingAbilitiesM :: (ConcreteDesignModel m, Reasons m) => (Attacker m) -> WithReason m (TamperingAbility m)
tamperingAbilitiesM = liftA2 TamperingAbilities tamperingAbilities

containerSecuredByMethodM :: (ConcreteDesignModel m, Reasons m) => ResourceContainer m -> WithReason m (TamperingAbility m)
containerSecuredByMethodM = liftA2 ContainerSecuredByMethod  containerSecuredByMethod

{-
interfacesAllowedToBeUsedBy :: Attacker m -> Set (Interface m)
dataAllowedToBeAccessedBy   :: Attacker m -> Set (DataSet m)
classificationOf  :: (Parameter m) -> Set (DataSet m)
classificationOfCall :: (Service m) -> Set (DataSet m)
-}
