{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE MonadComprehensions #-}

module InterfaceUsageExplicit where
import Data.Set.Monad as Set
import Palladio
import Security

import Control.Monad.Trans.Class(lift)

import Misc
import Reasons
import ReasonsModel


class (Ord (Interface m),
       BasicDesignModel m) => InterfaceUsageExplicit m where
  providedInterfacesAllowedToBeUsedBy :: Attacker m -> Set (Interface m)
  requiredInterfacesAllowedToBeUsedBy :: Attacker m -> Set (Interface m)


instance (BasicDesignModel m, InterfaceUsageExplicit m, Reasons m) => InterfaceUsage m where
    providedInterfacesDirectlyAccessibleTo = liftA2 InterfacesAllowedToBeUsedBy providedInterfacesAllowedToBeUsedBy
    requiredInterfacesDirectlyAccessibleTo = liftA2 InterfacesAllowedToBeUsedBy providedInterfacesAllowedToBeUsedBy
