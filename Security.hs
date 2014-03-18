{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
module Security where
import Data.Set as Set
import Palladio

(∈) :: Ord a => a -> Set a -> Bool
(∈) = Set.member

(∆) :: Set a -> [a]
(∆) = Set.elems

(⋅) :: Set a -> [a]
(⋅) = Set.elems

class (BasicDesignModel m) => AnalysisModel m where
  -- data AccessProof m
  dataAccessibleTo   :: Attacker m -> Set (DataSet m)

isSecureWithRespectTo :: (AnalysisModel m) => Attacker m -> Bool
isSecureWithRespectTo attacker = (dataAccessibleTo attacker) `isSubsetOf` (dataAllowedToBeAccessedBy attacker)

class (BasicDesignModel m) => AbstractDesignModel m where
  containersFullyAccessibleBy  :: Attacker m -> Set (ResourceContainer m)
  linksFullyAccessibleBy :: Attacker m -> Set (LinkingResource m)


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
                interface <- ((providedInterfacesOn container) `union` (requiredInterfacesOn container) ⋅),
                method    <- (methods interface⋅),
                parameter <- ((inputParameters method) `union` (outputParameters method) ⋅)
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
                parameter              <- ((inputParameters method) `union` (outputParameters method) ⋅)
  ]


observableMethods :: (AbstractDesignModel m) => (Attacker m) -> Set (Method m)
observableMethods attacker = fromList $
  [ method | interface <- elems $ interfacesAllowedToBeUsedBy attacker,
             interface ∈ systemRequires,
             method    <- elems $ methods interface
  ] ++
  [] -- TODO: Erweitern auf die Methoden deren Aufruf beobachtet werden kann, weil der Angreifer
     -- einen passenden Resourcecontainer oder LinkResource angreifen kann.


instance (AbstractDesignModel m) => AnalysisModel m where
  -- data AccessProof m = Unit
  dataAccessibleTo attacker = fromList $
    [ classificationOf parameter  | parameter <- (accessibleParameters attacker ⋅)] ++
    [ classificationOfCall method | method    <- (observableMethods attacker ⋅)]



data Sharing = OpenShared | ControlledExclusive -- | ControlledShared
  deriving Eq
data FurtherConnections = Possible | Existing | Complete
  deriving Eq


class (Ord (Location m),
       BasicDesignModel m) => ConcreteDesignModel m where
  data TamperingMethod m
  data Location m

  tamperingAbilities :: Attacker m -> Set (TamperingMethod m)

  locationsAccessibleBy :: Attacker m -> Set (Location m)

  containerTamperableByAttackerWithAbilities :: ResourceContainer m -> (Set (TamperingMethod m) -> Bool)
  linkTamperableByAttackerWithAbilities :: LinkingResource m -> (Set (TamperingMethod m) -> Bool)

  furtherConnections :: ResourceContainer m -> FurtherConnections
  sharing :: ResourceContainer m -> Sharing
  locality :: ResourceContainer m -> Location m

  linkLocality :: LinkingResource m -> Location m


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


class (Ord (DataSet m),
       PalladioComponentModel m) => BasicDesignModel m where
  data Attacker m
  data DataSet m


  datasets :: Set (DataSet m)
  attackers :: Set (Attacker m)

  interfacesAllowedToBeUsedBy :: Attacker m -> Set (Interface m)

  dataAllowedToBeAccessedBy :: Attacker m -> Set (DataSet m)

  classificationOf  :: (Parameter m) -> (DataSet m)
  classificationOfCall :: (Method m) -> (DataSet m)


instance (ConcreteDesignModel m) => AbstractDesignModel m where
 containersFullyAccessibleBy attacker = fromList $
    [ container | container <- (containersPhysicalAccessibleBy attacker ⋅),
                  containerTamperableByAttackerWithAbilities container (tamperingAbilities attacker)
    ] ++
    [ container | container <- (resourcecontainers ⋅),
                  sharing container == OpenShared
                  furtherConnections container == Existing,
    ] ++
    [ container | container <- (containersPhysicalAccessibleBy attacker ⋅),
                  sharing container == OpenShared
                  furtherConnections container == Possible,
    ]

 linksFullyAccessibleBy attacker =
    Set.fromList [ link | link <- (linksPhysicalAccessibleBy attacker⋅),
                          linkTamperableByAttackerWithAbilities link (tamperingAbilities attacker)
                 ]