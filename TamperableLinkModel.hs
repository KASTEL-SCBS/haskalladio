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
       ConcreteDesignModel m) => TamperAbilitiesLinkAccessModel m where
  linkPayloadSecuredByMethod  :: LinkingResource m -> Set (TamperingAbility m)
  linkMetaDataSecuredByMethod :: LinkingResource m -> Set (TamperingAbility m)

instance (TamperAbilitiesLinkAccessModel m, ConcreteDesignModel m, Reasons m) => LinkAccessModel m where
  exposesPhsicallyAccessiblePayloadTo link   =
    [ attacker | attacker <- lift $ attackers,
                 method    <- linkPayloadSecuredByMethodM link,
                 ability   <- tamperingAbilitiesM attacker,
                 method == ability
    ] `hence` (Inferred2 ExposesPhsicallyAccessiblePayloadTo link)

  exposesPhsicallyAccessibleMetaDataTo link  =
    [ attacker | attacker <- lift $ attackers,
                 method   <- linkMetaDataSecuredByMethodM link,
                 ability  <- tamperingAbilitiesM attacker,
                 method == ability
    ] `hence` (Inferred2 ExposesPhsicallyAccessibleMetaDataTo link)


linkMetaDataSecuredByMethodM :: (TamperAbilitiesLinkAccessModel m, Reasons m) => LinkingResource m -> WithReason m (TamperingAbility m)
linkMetaDataSecuredByMethodM = liftA2 LinkMetaDataSecuredByMethod  linkMetaDataSecuredByMethod

linkPayloadSecuredByMethodM :: (TamperAbilitiesLinkAccessModel m, Reasons m) => LinkingResource m -> WithReason m (TamperingAbility m)
linkPayloadSecuredByMethodM = liftA2 LinkPayloadSecuredByMethod  linkPayloadSecuredByMethod


