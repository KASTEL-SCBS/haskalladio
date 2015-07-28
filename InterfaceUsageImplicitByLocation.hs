{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE MonadComprehensions #-}

module InterfaceUsageImplicitByLocation where
import Data.Set.Monad as Set
import Palladio
import AbstractAnalysis(providedInterfacesOnM,requiredInterfacesOnM)
import Security

import Control.Monad.Trans.Class(lift)

import Misc
import Reasons
import ReasonsModel


instance (ConcreteDesignModel m, Reasons m) => InterfaceUsage m where
    providedInterfacesDirectlyAccessibleTo attacker =
      [ interface | (container,_) <- containersPhysicalAccessibleBy attacker,
                    interface <- providedInterfacesOnM container

      ] `hence` (Inferred2 InterfacesDirectlyAccessibleTo attacker)

    requiredInterfacesDirectlyAccessibleTo attacker =
      [ interface | (container,_) <- containersPhysicalAccessibleBy attacker,
                    interface <- requiredInterfacesOnM container

      ] `hence` (Inferred2 InterfacesDirectlyAccessibleTo attacker)

