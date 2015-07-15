{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE MonadComprehensions #-}

module SimpleLinkModelWithExceptions where
import Data.Set.Monad as Set
import Palladio
import Security

import Control.Monad.Trans.Class(lift)

import Misc
import Reasons
import ReasonsModel

class (Ord (Location m),
       BasicDesignModel m) => SimpleEncryptionWithExceptionsLinkAccessModel m where
  isEncryptedButFor :: LinkingResource m -> Set (DataSet m)

instance (SimpleEncryptionWithExceptionsLinkAccessModel m, Reasons m) => LinkAccessModel m where
  exposesPhsicallyAccessibleDataTo link =
    [ (attacker, dataset) | dataset  <- lift $ datasets,
                            _        <- notEncryptedButForM link dataset,
                            attacker <- lift $ attackers
    ] `hence` (Inferred2 ExposesPhsicallyAccessibleDataTo link)

notEncryptedButForM :: (SimpleEncryptionWithExceptionsLinkAccessModel m, Reasons m) => LinkingResource m -> DataSet m ->  WithReason m ()
notEncryptedButForM = liftNot2 EncryptedBut isEncryptedButFor
