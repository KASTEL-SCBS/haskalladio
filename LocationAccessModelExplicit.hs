{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE MonadComprehensions #-}

module LocationAccessModelExplicit where
import Data.Set.Monad as Set
import Palladio
import Security

import Control.Monad.Trans.Class(lift)

import Misc
import Reasons
import ReasonsModel


class (Ord (Interface m),
       ConcreteDesignModel m) => LocationAccessModelExplicit m where
  locationsAccessibleByExplicit :: Attacker m -> Set (Location m)


instance (BasicDesignModel m, LocationAccessModelExplicit m, Reasons m) => LocationAccessModel m where
  locationsAccessibleBy = liftA2 LocationsAccessibleBy locationsAccessibleByExplicit
