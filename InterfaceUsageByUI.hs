{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ConstraintKinds #-}

module InterfaceUsageByUI where
import Data.Set.Monad as Set
import Palladio
import Security
import AbstractAnalysis(providedInterfacesOnM,requiredInterfacesOnM)
import Control.Monad.Trans.Class(lift)

import Misc
import Reasons
import ReasonsModel


class (BasicDesignModel m) => InterfaceUsageByUI m where
  uiInterfaceOn     :: ResourceContainer m -> Set (Interface m)

instance (ConcreteDesignModel m, InterfaceUsageByUI m, LocationAccessModel m, Reasons m) => InterfaceUsage m where
    providedInterfacesDirectlyAccessibleTo attacker =
      [ interface | (container,location) <- containersPhysicalAccessibleBy attacker,
                    interface  <- providedInterfacesOnM container,
                    interface' <- uiInterfaceOnM container, -- TODO: durch passende
                    interface == interface'                 -- wellformedness Bedingung ersetzen
      ] `hence` (Inferred2 InterfacesDirectlyAccessibleTo attacker)

    requiredInterfacesDirectlyAccessibleTo attacker =
      [ interface | (container,location) <- containersPhysicalAccessibleBy attacker,
                    interface  <- requiredInterfacesOnM container,
                    interface' <- uiInterfaceOnM container, -- TODO: durch passende
                    interface == interface'                -- wellformedness Bedingung ersetzen
      ] `hence` (Inferred2 InterfacesDirectlyAccessibleTo attacker)


uiInterfaceOnM :: (InterfaceUsageByUI m, Reasons m) => ResourceContainer m -> WithReason m (Interface m)
uiInterfaceOnM = liftA2 UiInterfaceOn uiInterfaceOn
