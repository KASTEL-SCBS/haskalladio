{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE MonadComprehensions #-}

module InterfaceUsageByGroup where
import Data.Set.Monad as Set
import Palladio
import Security

import Control.Monad.Trans.Class(lift)

import Misc
import Reasons
import ReasonsModel


class (Ord (Interface m),
       BasicDesignModel m) => InterfaceUsageByPassword m where
  data AccessControlDomain m
  data UserGroup m

  isProtectedIn      :: Interface m -> AccessControlDomain m
  hasAccessToDomains :: UserAccount m -> Set (AccessControlDomain m)


  isMemberOfGroups   :: Attacker m -> Set (AccessControlDomain m)



instance (BasicDesignModel m, InterfaceUsageByPassword m, Reasons m) => InterfaceUsage m where
    providedInterfacesDirectlyAccessibleTo = liftA2 InterfacesAllowedToBeUsedBy providedInterfacesAllowedToBeUsedBy
    requiredInterfacesDirectlyAccessibleTo = liftA2 InterfacesAllowedToBeUsedBy providedInterfacesAllowedToBeUsedBy

  providedInterfacesAllowedToBeUsedBy :: Attacker m -> Set (Interface m)
  requiredInterfacesAllowedToBeUsedBy :: Attacker m -> Set (Interface m)
