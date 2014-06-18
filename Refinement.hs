{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
module Security where
import Data.Set as Set
import Palladio

import Misc

class PalladioComponentModel m  =>  LinkRefinement m where
  refinementOf :: LinkingRessource m -> Maybe (Component m)


allRefinementValid :: Bool
allRefinementValid = (∀) (\link ->  case (refinementOf link) of
  Nothing -> True
  Just component -> isValidRefinement link component)
  linkingresources

isValidRefinement :: LinkingRessource m -> Component m -> Bool
isValidRefinement = undefined







