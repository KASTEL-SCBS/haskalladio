{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Security where
import Data.Set as Set
import Palladio

import Misc


{- Der Sicherheitsbegriff -}
class (BasicDesignModel m) => (SecurityProperty m) where
  isSecureWithRespectTo ::  Attacker m -> Bool


{- So soll im einfachsten Fall ein Analyseresult eines Modells aussehen.
   * Mögliche Erweiterung: Berechnung eines "schwächsten Angreifers":
      weakestAttackerWithAccessTo :: DataSet m -> Attacker

   * Mögliche Erweiterung: "Beweis" der nachvollziehbar macht, wie und warum ein Angreifer Zugruff hat:
      dataAccessibleTo   :: Attacker m -> Set ((DataSet m, AccessProof m))

   * Mögliche Erweiterung: "kleinste Menge an Änderungen" die Notwendig sind, um das System abzusichern
      dataAccessibleTo   :: Attacker m -> Set ((DataSet m, ChangeRequest m))
-}
class (BasicDesignModel m) => AnalysisResult m where
  dataAccessibleTo   :: Attacker m -> Set (DataSet m)

instance (AnalysisResult m) => (SecurityProperty m) where
  isSecureWithRespectTo attacker = (dataAccessibleTo attacker) ⊆ (dataAllowedToBeAccessedBy attacker)



{- Egal wie genau nun ein Sicherheitsmodell aussieht (sharing,locations etc pp),
   es muss mindestens folgende Dinge spezifizieren:
-}
class (Ord (DataSet m),
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
  containersFullyAccessibleBy  :: Attacker m -> Set (ResourceContainer m)
  linksMetaDataFullyAccessibleBy       :: Attacker m -> Set (LinkingResource m)
  linksPayloadFullyAccessibleBy       :: Attacker m -> Set (LinkingResource m)







{- Eine Variante eines konkreten Sicherheitsmodells -}
data Sharing = OpenShared
             | ControlledExclusive
             deriving Eq

data FurtherConnections = Possible
                        | Existing
                        | Complete
                        deriving Eq


class (Ord (Location m),
       BasicDesignModel m) => LinkAccessModel m where
  -- linksExposingPayloadWhenPysicalliAccessibleTo
  exposesPhsicallyAccessiblePayloadTo  :: LinkingResource m -> Attacker m ->  Bool
  exposesPhsicallyAccessibleMetaDataTo :: LinkingResource m -> Attacker m ->  Bool


class (Ord (Location m),
       BasicDesignModel m) => ConcreteDesignModel m where
  data TamperingAbility m
  data Location m

  tamperingAbilities    :: Attacker m -> Set (TamperingAbility m)

  locationsAccessibleBy :: Attacker m -> Set (Location m)

  containerTamperableByAttackerWithAbilities :: ResourceContainer m -> Set (TamperingAbility m) -> Bool

  furtherConnections :: ResourceContainer m -> FurtherConnections
  sharing            :: ResourceContainer m -> Sharing
  location           :: ResourceContainer m -> Location m

  linkLocation       :: LinkingResource m -> Set (Location m)

  
{- Damit erhält man ein Analyseergebnis folgendermaßen: -}
instance (ConcreteDesignModel m, LinkAccessModel m) => AbstractDesignModel m where
 containersFullyAccessibleBy attacker = fromList $
    [ container | container <- (containersPhysicalAccessibleBy attacker ⋅),
                  containerTamperableByAttackerWithAbilities container (tamperingAbilities attacker)
    ] ++
    [ container | container <- (resourcecontainers ⋅),
                  sharing container == OpenShared,
                  furtherConnections container == Existing
    ] ++
    [ container | container <- (containersPhysicalAccessibleBy attacker ⋅),
                  sharing container == OpenShared,
                  furtherConnections container == Possible
    ]

 linksPayloadFullyAccessibleBy attacker =
    Set.fromList [ link | link <- (linksPhysicalAccessibleBy attacker⋅),
                          link `exposesPhsicallyAccessiblePayloadTo` attacker
                 ]

 linksMetaDataFullyAccessibleBy attacker =
    Set.fromList [ link | link <- (linksPhysicalAccessibleBy attacker⋅),
                          link `exposesPhsicallyAccessibleMetaDataTo` attacker
                 ]



linksPhysicalAccessibleBy      :: (ConcreteDesignModel m ) => Attacker m -> Set (LinkingResource m)
linksPhysicalAccessibleBy attacker =
  Set.fromList [ link | link              <- (linkingresources ⋅),
                        not $ isEmpty (linkLocation link ∩ locationsAccessibleBy attacker)
               ]

containersPhysicalAccessibleBy :: (ConcreteDesignModel m ) => Attacker m -> Set (ResourceContainer m)
containersPhysicalAccessibleBy attacker =
  Set.fromList [ container | container          <- (resourcecontainers ⋅),
                             location container ∈ locationsAccessibleBy attacker
               ]



