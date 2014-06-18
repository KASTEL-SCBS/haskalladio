{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
module SimpleLinkModel where
import Data.Set as Set
import Palladio
import Security

import Misc


class (Ord (Location m),
       BasicDesignModel m) => SimpleEncryptionLinkAccessModel m where
  isEncrypted :: LinkingResource m -> Bool

instance (SimpleEncryptionLinkAccessModel m) => LinkAccessModel m where
  link `exposesPhsicallyAccessiblePayloadTo`  attacker = (not.isEncrypted) link
  link `exposesPhsicallyAccessibleMetaDataTo` attacker = True

