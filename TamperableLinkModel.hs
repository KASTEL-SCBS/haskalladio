{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE MonadComprehensions #-}

module TamperableLinkModel where
import Data.Set.Monad as Set
import Palladio
import Security

import Control.Monad.Trans.Class(lift)

import Misc
import Reasons
import ReasonsModel

class (Ord (Location m),
       BasicDesignModel m) => TamperAbilitiesLinkAccessModel m where
  linkTamperableByAttackerWithAbilities :: LinkingResource m -> Set (TamperingAbility m) -> Bool

instance (TamperAbilitiesLinkAccessModel m, ConcreteDesignModel m, Reasons m) => LinkAccessModel m where
  exposesPhsicallyAccessiblePayloadTo link   =
    [ attacker | attacker <- lift $ attackers,
                 linkTamperableByAttackerWithAbilities link (tamperingAbilities attacker)
    ] `hence` (Inferred2 ExposesPhsicallyAccessiblePayloadTo link)

  exposesPhsicallyAccessibleMetaDataTo link  =
    [ attacker | attacker <- lift $ attackers,
                 linkTamperableByAttackerWithAbilities link (tamperingAbilities attacker)
    ] `hence` (Inferred2 ExposesPhsicallyAccessibleMetaDataTo link)
