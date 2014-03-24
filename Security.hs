{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
module Security where
import Data.Set as Set
import Palladio

import Misc

class ComponentRepository m => InformationFlowSpecification m where
  possiblyInfluencedBy      :: Parameter m -> Set (Parameter m)
  callsPossiblyInfluencedBy :: Parameter m -> Set (Method m)

  possiblyInfluencedByCall      :: Method m -> Set (Parameter m)
  callsPossiblyInfluencedByCall :: Method m -> Set (Method m)


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

{- Der Sicherheitsbegriff -}
isSecureWithRespectTo :: (AnalysisResult m) => Attacker m -> Bool
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

  classificationOf  :: (Parameter m) -> (DataSet m)
  classificationOfCall :: (Method m) -> (DataSet m)



{- Egal wie genau nun ein Sicherheitsmodell aussieht (sharing,locations etc pp),
   es muss möglich sein, damit Folgende Dinge herauszufinden:
-}
class (BasicDesignModel m) => AbstractDesignModel m where
  containersFullyAccessibleBy  :: Attacker m -> Set (ResourceContainer m)
  linksFullyAccessibleBy       :: Attacker m -> Set (LinkingResource m)

{- Denn damit kann man direkt ein Analyserusultat bestimmen: -}
instance (AbstractDesignModel m) => AnalysisResult m where
  dataAccessibleTo attacker = fromList $
    [ classificationOf parameter  | parameter <- (accessibleParameters attacker ⋅)] ++
    [ classificationOfCall method | method    <- (observableMethods attacker ⋅)]






{- Eine Variante eines konkreten Sicherheitsmodells -}
data Sharing = OpenShared
             | ControlledExclusive
          -- | ControlledShared
             deriving Eq
-- TODO: was bedeutet ControlledShared

data FurtherConnections = Possible
                        | Existing
                        | Complete
                        deriving Eq

class (Ord (Location m),
       BasicDesignModel m) => ConcreteDesignModel m where
  data TamperingMethod m
  data Location m

  tamperingAbilities    :: Attacker m -> Set (TamperingMethod m)

  locationsAccessibleBy :: Attacker m -> Set (Location m)

  containerTamperableByAttackerWithAbilities :: ResourceContainer m -> Set (TamperingMethod m) -> Bool
  linkTamperableByAttackerWithAbilities      :: LinkingResource m   -> Set (TamperingMethod m) -> Bool

  furtherConnections :: ResourceContainer m -> FurtherConnections
  sharing            :: ResourceContainer m -> Sharing
  locality           :: ResourceContainer m -> Location m

  linkLocality       :: LinkingResource m -> Location m

{- Damit erhält man ein Analyseergebnis folgendermaßen: -}
instance (ConcreteDesignModel m) => AbstractDesignModel m where
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

 linksFullyAccessibleBy attacker =
    Set.fromList [ link | link <- (linksPhysicalAccessibleBy attacker⋅),
                          linkTamperableByAttackerWithAbilities link (tamperingAbilities attacker)
                 ]

{- ... unter Verwendung folgender "Hilfsbegriffe" ... -}
accessibleParameters :: (AbstractDesignModel m) => (Attacker m) -> Set (Parameter m)
accessibleParameters attacker = fromList $
  -- Ausgabe-Parameter, auf die der Angreifer als regulärer "Benutzer" des Systems Zugriff hat
  [ parameter | interface <- (interfacesAllowedToBeUsedBy attacker ⋅),
                interface ∈  systemProvides,
                method    <- (methods interface⋅),
                parameter <- (outputParameters method⋅)
  ] ++

  -- Eingabe-Parameter,auf die der Angreifer als regulärer "Benutzer" des Systems Zugriff hat, weil er vom System Aufgerufen wird.
  [ parameter | interface <- (interfacesAllowedToBeUsedBy attacker ⋅),
                interface ∈  systemRequires,
                method    <- (methods interface ⋅),
                parameter <- (inputParameters method ⋅)
  ] ++

  -- Parameter, auf die der Angreifer Zugriff hat, weil er einen entsprechenden ResourceContainer angreifen konnte.
  [ parameter | container <- (containersFullyAccessibleBy attacker ⋅),
                interface <- ((providedInterfacesOn container) ∪ (requiredInterfacesOn container) ⋅),
                method    <- (methods interface⋅),
                parameter <- ((inputParameters method) ∪ (outputParameters method) ⋅)
  ] ++

  -- Parameter, auf die der Angreifer Zugriff hat, weil er eine entsprechende LinkResource angreifen konnte.
  [ parameter | link                  <- (linksFullyAccessibleBy attacker⋅),
                let (containerLeft,
                     containerRight)   = linkBetween link,
                left                   <- (assembliesOn containerLeft ⋅),
                interface              <- (requires (componentOf left) ⋅),
                let (ByAssembly right) = systemAssembledTo left interface,
                right                  ∈  assembliesOn containerRight,
                method                 <- (methods interface ⋅),
                parameter              <- ((inputParameters method) ∪ (outputParameters method) ⋅)
  ]


observableMethods :: (AbstractDesignModel m) => (Attacker m) -> Set (Method m)
observableMethods attacker = fromList $
  [ method | interface <- elems $ interfacesAllowedToBeUsedBy attacker,
             interface ∈ systemRequires,
             method    <- elems $ methods interface
  ] ++
  [] -- TODO: Erweitern auf die Methoden deren Aufruf beobachtet werden kann, weil der Angreifer
     -- einen passenden Resourcecontainer oder LinkResource angreifen kann.



linksPhysicalAccessibleBy      :: (ConcreteDesignModel m ) => Attacker m -> Set (LinkingResource m)
linksPhysicalAccessibleBy attacker =
  Set.fromList [ link | link              <- (linkingresources ⋅),
                        linkLocality link ∈ locationsAccessibleBy attacker
               ]

containersPhysicalAccessibleBy :: (ConcreteDesignModel m ) => Attacker m -> Set (ResourceContainer m)
containersPhysicalAccessibleBy attacker =
  Set.fromList [ container | container          <- (resourcecontainers ⋅),
                             locality container ∈ locationsAccessibleBy attacker
               ]





