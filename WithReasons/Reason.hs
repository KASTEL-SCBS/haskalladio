{-# LANGUAGE MonadComprehensions #-}
module Reason where

import Control.Monad.Trans.Writer.Lazy
import Control.Monad(msum)
import Control.Monad.Trans.Class(lift)

import Test.QuickCheck

data Person = Inge | Emil | Petra | Willi | Kunibert | Fritz deriving (Eq,Ord,Show)


age Fritz = 30
age _     = 90

mothers Emil = [Inge]
mothers Petra = [Inge]
mothers Willi = [Petra]
mothers _  = []

fathers Emil = [Fritz]
fathers Kunibert = [Emil]
fathers _ = []

parents      person = [ m | m <- mothers person] ++ [ f | f <- fathers person]
grandparents person = [ p'' | p' <- parents person, p'' <- parents p', age p'' > 60 ]





data Relation = Mothers | Fathers | Parents | Grantparents deriving (Eq,Ord,Show)
data Reason = Axiom Relation Person Person deriving (Show)
type WithReason a = WriterT [Reason] ([]) a

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

withoutReasons :: WithReason a -> [a]
withoutReasons = map fst . runWriterT

correct person =
     (withoutReasons $ parentsM person)      == parents person
  && (withoutReasons $ grandparentsM person) == grandparents person

instance Arbitrary Person where
  arbitrary = elements [Inge, Emil, Petra, Willi, Kunibert, Fritz]
