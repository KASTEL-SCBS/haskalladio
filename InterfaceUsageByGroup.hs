{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ConstraintKinds #-}

module InterfaceUsageByGroup where
import Data.Set.Monad as Set
import Palladio
import Security
import AbstractAnalysis(providedInterfacesOnM,requiredInterfacesOnM)
import Control.Monad.Trans.Class(lift)

import Misc
import Reasons
import ReasonsModel


class (ReasonLike (AccessControlDomain m),
       ReasonLike (UserGroup m),
       BasicDesignModel m) => InterfaceUsageByGroup m where
  data AccessControlDomain m
  data UserGroup m

  isInDomain         :: Interface m -> AccessControlDomain m
  hasAccessToDomains :: UserGroup m -> Set (AccessControlDomain m)
  isMemberOfGroups   :: Attacker m -> Set (UserGroup m)




instance (ConcreteDesignModel m, InterfaceUsageByGroup m, LocationAccessModel m, Reasons m) => InterfaceUsage m where
    providedInterfacesDirectlyAccessibleTo attacker =
      [ interface | (container,location) <- containersPhysicalAccessibleBy attacker,
                    interface <- providedInterfacesOnM container,
                    domain <- isInDomainM interface,
                    group <- isMemberOfGroupsM attacker,
                    domain' <- hasAccessToDomainsM group,
                    domain == domain'
      ] `hence` (Inferred2 InterfacesDirectlyAccessibleTo attacker)

    requiredInterfacesDirectlyAccessibleTo attacker =
      [ interface | (container,location) <- containersPhysicalAccessibleBy attacker,
                    interface <- requiredInterfacesOnM container,
                    domain <- isInDomainM interface,
                    group <- isMemberOfGroupsM attacker,
                    domain' <- hasAccessToDomainsM group,
                    domain == domain'
      ] `hence` (Inferred2 InterfacesDirectlyAccessibleTo attacker)




isInDomainM :: (InterfaceUsageByGroup m, Reasons m) => (Interface m) -> WithReason m (AccessControlDomain m)
isInDomainM = liftF IsInDomain isInDomain


hasAccessToDomainsM :: (InterfaceUsageByGroup m, Reasons m) => UserGroup m -> WithReason m (AccessControlDomain m)
hasAccessToDomainsM = liftA2 HasAccessToDomains hasAccessToDomains

isMemberOfGroupsM :: (InterfaceUsageByGroup m, Reasons m) => Attacker m -> WithReason m (UserGroup m)
isMemberOfGroupsM = liftA2 IsMemberOfGroups isMemberOfGroups
