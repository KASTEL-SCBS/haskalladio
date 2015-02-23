{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module WithReasons.Relartives where

import Prelude hiding (map)
import Control.Monad.Trans.Writer.Lazy
import Control.Monad(msum)
import Control.Monad.Trans.Class(lift)

import Data.Set.Monad

import Test.QuickCheck
import Data.Typeable

import Reasons

data Person = Hannes | Inge | Emil | Petra | Willi | Kunibert | Fritz deriving (Eq,Ord,Show, Typeable)

age :: Person -> Integer
age Emil = 16
age Hannes = 16
age Kunibert = 60
age Fritz    = 80
age _        = 90

mothers Emil   = fromList [Inge]
mothers Hannes = mothers Emil

mothers Petra = fromList [Inge]
mothers Willi = fromList [Petra]
mothers Kunibert = fromList [Petra]
mothers _  = fromList []

fathers Emil = fromList [Fritz]
fathers Hannes = fathers Emil

fathers Kunibert = fromList [Emil]
fathers _ = fromList []

parents      person = [ m | m <- mothers person] `union` [ f | f <- fathers person]
grandparents person = [ p'' | p' <- parents person, p'' <- parents p', age p'' > 60 ]


data Relatives = Relatives deriving Typeable

instance Reasons Relatives where
  data Relation Relatives = Mothers | Fathers | Parents | Grantparents deriving (Eq,Ord,Show,Typeable)
  data Function Relatives = Age deriving (Eq,Ord,Show,Typeable)

mothersM = liftR2 Mothers mothers
fathersM = liftR2 Fathers fathers
ageM = liftF Age age







parentsM :: Person -> WithReason Relatives Person
parentsM person = msum [ [ m | m <- mothersM person ],
                         [ f | f <- fathersM person ]
                       ]

grandparentsM :: Person -> WithReason Relatives Person
grandparentsM person = [ p'' | p' <- parentsM person, p'' <- parentsM p', a <- ageM p'', a > 60 ]


youngerthan70 :: Person -> WithReason Relatives ()
youngerthan70 person = [ () |  a <- ageM person, a < 70]


twins :: Person -> Person -> WithReason Relatives ()
twins  p1 p2  = [ () | p1 /= p2,
                       m1 <- mothersM p1, m2 <- mothersM p2, m1==m2,
                       f1 <- fathersM p1, f2 <- fathersM p2, f1==f2,
                       a1 <- ageM p1, a2 <- ageM p2, a1 == a2]

correct person =
     (withoutReasons $ parentsM person)      == parents person
  && (withoutReasons $ grandparentsM person) == grandparents person

instance Arbitrary Person where
  arbitrary = elements [Inge, Emil, Petra, Willi, Kunibert, Fritz]

