{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module ReasonsModel where
import Data.Typeable

import Reasons


-- TODO: Find out WhyTheFuck ghc requires *m* has to be Typable,
-- when all we need to be Typable is *Relation m* and *Function m*
instance (Typeable m) => Reasons m where
  data Relation m = DataAllowedToBeAccessedBy
                  | DataAccessibleTo
                  | ClassificationOfCall
                  | ClassificationOf
                  | InterfacesAllowedToBeUsedBy
                  | ProvidedInterfacesOn
                  | RequiredInterfacesOn
                  | LinksPayloadFullyAccessibleBy
                  | LinksMetaDataFullyAccessibleBy
                  | LinksPhysicallyAccessibleBy
                  | LinkPayloadSecuredByMethod
                  | LinkMetaDataSecuredByMethod
                  | ContainerFullyAccessibleBy
                  | ContainerPhysicallyAccessibleBy
                  | ContainerSecuredByMethod
                  | TamperingAbilities
                  | LocationsAccessibleBy
                  | LinkLocation
                  | Location
                  | ObservableServices
                  | AccessibleParameters
                  | ExposesPhsicallyAccessiblePayloadTo
                  | ExposesPhsicallyAccessibleMetaDataTo
                  deriving (Eq,Ord,Show,Typeable)
  data Function m = Sharing
                  | FurtherConnections
                  | IsEncrypted
                  deriving (Eq,Ord,Show,Typeable)

