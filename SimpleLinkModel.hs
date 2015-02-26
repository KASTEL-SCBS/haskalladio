{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE MonadComprehensions #-}

module SimpleLinkModel where
import Data.Set as Set
import Palladio
import Security

import Control.Monad.Trans.Class(lift)

import Misc
import Reasons
import ReasonsModel

class (Ord (Location m),
       BasicDesignModel m) => SimpleEncryptionLinkAccessModel m where
  isEncrypted :: LinkingResource m -> Bool

instance (SimpleEncryptionLinkAccessModel m, Reasons m) => LinkAccessModel m where
  exposesPhsicallyAccessiblePayloadTo link   =
    [ attacker | encrypted <- isEncryptedM link,
                 not encrypted,
                 attacker <- lift $ attackers
    ] `hence` (Inferred2 ExposesPhsicallyAccessiblePayloadTo link)

  exposesPhsicallyAccessibleMetaDataTo link  =
    [ attacker | attacker <- lift $ attackers
    ] `hence` (Inferred2 ExposesPhsicallyAccessibleMetaDataTo link)


isEncryptedM :: (SimpleEncryptionLinkAccessModel m, Reasons m) => (LinkingResource m) -> WithReason m Bool
isEncryptedM = liftF IsEncrypted isEncrypted
