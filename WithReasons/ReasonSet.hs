{-# LANGUAGE MonadComprehensions #-}
module ReasonSet where

import Prelude hiding (map)
import Control.Monad.Trans.Writer.Lazy
import Control.Monad(msum)
import Control.Monad.Trans.Class(lift)

import Data.Set.Monad

import Test.QuickCheck

data Person = Inge | Emil | Petra | Willi | Kunibert | Fritz deriving (Eq,Ord,Show)


age Fritz = 30
age _     = 90

mothers Emil = fromList [Inge]
mothers Petra = fromList [Inge]
mothers Willi = fromList [Petra]
mothers _  = fromList []

fathers Emil = fromList [Fritz]
fathers Kunibert = fromList [Emil]
fathers _ = fromList []

parents      person = [ m | m <- mothers person] `union` [ f | f <- fathers person]
grandparents person = [ p'' | p' <- parents person, p'' <- parents p', age p'' > 60 ]




data Relation = Mothers | Fathers | Parents | Grantparents deriving (Eq,Ord,Show)
data Reason = Axiom Relation Person Person deriving (Eq, Ord, Show)
type WithReason a = WriterT [Reason] Set a

mothersM :: Person -> WithReason Person
mothersM person = do
   m <- lift $ mothers person
   tell $ [Axiom Mothers person m]
   return m



fathersM :: Person -> WithReason Person
fathersM person = do
   f <- lift $ fathers person
   tell $ [Axiom Fathers person f]
   return f


parentsM :: Person -> WithReason Person
parentsM person = msum [ [ m | m <- mothersM person ],
                         [ f | f <- fathersM person ]
                       ]

grandparentsM :: Person -> WithReason Person
grandparentsM person = [ p'' | p' <- parentsM person, p'' <- parentsM p', age p'' > 60 ]

withoutReasons :: Ord a => WithReason a -> Set a
withoutReasons = map fst . runWriterT

correct person =
     (withoutReasons $ parentsM person)      == parents person
  && (withoutReasons $ grandparentsM person) == grandparents person

instance Arbitrary Person where
  arbitrary = elements [Inge, Emil, Petra, Willi, Kunibert, Fritz]
