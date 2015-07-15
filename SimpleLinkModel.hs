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
  exposesPhsicallyAccessibleDataTo link =
    [ (attacker, dataset) | encrypted <- isEncryptedM link,
                            not encrypted,
                            attacker <- lift $ attackers,
                            dataset  <- lift $ datasets
    ] `hence` (Inferred2 ExposesPhsicallyAccessibleDataTo link)

isEncryptedM :: (SimpleEncryptionLinkAccessModel m, Reasons m) => (LinkingResource m) -> WithReason m Bool
isEncryptedM = liftF IsEncrypted isEncrypted
