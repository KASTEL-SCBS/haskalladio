{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
module TamperableLinkModel where
import Data.Set as Set
import Palladio
import Security

import Misc


class (Ord (Location m),
       BasicDesignModel m) => TamperAbilitiesLinkAccessModel m where
  linkTamperableByAttackerWithAbilities :: LinkingResource m -> Set (TamperingMethod m) -> Bool

instance (TamperAbilitiesLinkAccessModel m, ConcreteDesignModel m) => LinkAccessModel m where
  link `exposesPhsicallyAccessiblePayloadTo`  attacker = linkTamperableByAttackerWithAbilities link (tamperingAbilities attacker)
  link `exposesPhsicallyAccessibleMetaDataTo` attacker = linkTamperableByAttackerWithAbilities link (tamperingAbilities attacker)
