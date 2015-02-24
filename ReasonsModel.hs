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
                    deriving (Eq,Ord,Show,Typeable)
  data Function m = Age deriving (Eq,Ord,Show,Typeable)

