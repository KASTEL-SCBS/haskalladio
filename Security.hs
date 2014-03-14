{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Security where
import Data.Set as Set
import Palladio

class (BasicDesignModel m) => AnalysisModel m where
  data AccessProof m
  hasAccesTo   :: Attacker m -> Set ((DataSet m, AccessProof m))
  isAllowedAccesTo :: Attacker m -> Set (DataSet m)


class (BasicDesignModel m) => AbstractDesignModel m where
  containersFullyAccessibleBy  :: Attacker m -> Set (ResourceContainer m)
  linksFullyAccessibleBy :: Attacker m -> Set (LinkingResource m)


accessibleParameters :: (AbstractDesignModel m) => (Attacker m) -> Set (Parameter m)
accessibleParameters attacker = fromList $
  [ parameter | interface <- elems $ interfacesAccessibleBy attacker,
                interface `Set.member` systemProvides,
                method <- elems $ methods interface,
                parameter <- elems $ outputParameters method
  ] ++
  [ parameter | interface <- elems $ interfacesAccessibleBy attacker,
                interface `Set.member` systemRequires,
                method <- elems $ methods interface,
                parameter <- elems $ inputParameters method
  ]


observableMethods :: (AbstractDesignModel m) => (Attacker m) -> Set (Method m)
observableMethods attacker = fromList $
  [ method | interface <- elems $ interfacesAccessibleBy attacker,
                interface `Set.member` systemRequires,
                method <- elems $ methods interface
  ]


data Sharing = OpenShared | ControlledExclusive | ControlledShared
data FurtherConnections = Possible | Existing | Complete


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
  Set.fromList [ link | link     <- elems linkingresources,
                        linkLocality link `Set.member` locationsAccessibleBy attacker
               ]

containersPhysicalAccessibleBy :: (ConcreteDesignModel m ) => Attacker m -> Set (ResourceContainer m)
containersPhysicalAccessibleBy attacker =
  Set.fromList [ container | container <- elems resourcecontainers,
                             locality container `Set.member` locationsAccessibleBy attacker
               ]


class (PalladioComponentModel m) => BasicDesignModel m where
  data Attacker m
  data DataSet m

  datasets :: Set (DataSet m)
  attackers :: Set (Attacker m)

  interfacesAccessibleBy :: Attacker m -> Set (Interface m)

  --classifiedAs :: (Parameter m) -> (DataSet m)


instance (ConcreteDesignModel m) => AbstractDesignModel m where
 containersFullyAccessibleBy attacker =
    Set.fromList [ container | container <- elems $ containersPhysicalAccessibleBy attacker,
                               containerTamperableByAttackerWithAbilities container (tamperingAbilities attacker)
                 ]
 linksFullyAccessibleBy attacker =
    Set.fromList [ link | link <- elems $ linksPhysicalAccessibleBy attacker,
                          linkTamperableByAttackerWithAbilities link (tamperingAbilities attacker)
                 ]