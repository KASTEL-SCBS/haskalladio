{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}


module WithReasons.ReasonSet where

import Prelude hiding (map)
import Control.Monad.Trans.Writer.Lazy
import Control.Monad(msum)
import Control.Monad.Trans.Class(lift)

import Data.Set.Monad

import Test.QuickCheck
import Data.Typeable

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




data Relation = Mothers | Fathers | Parents | Grantparents deriving (Eq,Ord,Show,Typeable)
data Function = Age deriving (Eq,Ord,Show,Typeable)

data Reason  where
  OldEnough :: Person -> Reason
  Axiom1 :: (Typeable s, Show s, Ord s) => Relation -> s      -> Reason
  Axiom2 :: (Typeable s, Show s, Ord s,
             Typeable t, Show t, Ord t) => Relation -> s -> t -> Reason
  MapsTo :: (Typeable s, Show s, Ord s,
             Typeable t, Show t, Ord t) => Function -> s -> t -> Reason
deriving instance Show Reason

instance Eq Reason where
  OldEnough p   == OldEnough p'    = p == p
  Axiom1 r x    == Axiom1 r' x'    = r == r' && Just x == cast x'
  Axiom2 r x y  == Axiom2 r' x' y' = r == r' && Just x == cast x' && Just y == cast y'
  _             == _               = False

instance Ord Reason where
  OldEnough p   <= OldEnough p'    = p <= p
  Axiom1 r x    <= Axiom1 r' x'    = r <  r' || (r == r' && ( Just x <= cast x' ))
  Axiom2 r x y  <= Axiom2 r' x' y' = r <  r' || (r == r' && ((Just x <= cast x') || (Just x == cast x') && Just y <= cast y'))
  OldEnough _   <= _               = True
  _             <= OldEnough _     = False
  Axiom1 _ _    <= _               = True
  _             <= Axiom1 _ _      = False
  Axiom2 _ _ _  <= _               = True
  _             <= Axiom2 _ _ _    = False


type WithReason a = WriterT [Reason] Set a

because :: [Reason] -> WithReason ()
because = tell

mothersM :: Person -> WithReason Person
mothersM person = do
   m <- lift $ mothers person
   tell $ [Axiom2 Mothers person m]
   return m


fathersM :: Person -> WithReason Person
fathersM person = do
   f <- lift $ fathers person
   tell $ [Axiom2 Fathers person f]
   return f

ageM :: Person -> WithReason Integer
ageM person = do
   tell $ [MapsTo Age person (age person)]
   return (age person)

parentsM :: Person -> WithReason Person
parentsM person = msum [ [ m | m <- mothersM person ],
                         [ f | f <- fathersM person ]
                       ]

grandparentsM :: Person -> WithReason Person
grandparentsM person = [ p'' | p' <- parentsM person, p'' <- parentsM p', age p'' > 60,
                                _ <- because [OldEnough p'']
                       ]


youngerthan70 :: Person -> WithReason ()
youngerthan70 person = [ () |  a <- ageM person, a < 70]


twins :: Person -> Person -> WithReason ()
twins  p1 p2  = [ () | p1 /= p2,
                       m1 <- mothersM p1, m2 <- mothersM p2, m1==m2,
                       f1 <- fathersM p1, f2 <- fathersM p2, f1==f2,
                       a1 <- ageM p1, a2 <- ageM p2, a1 == a2]


withoutReasons :: Ord a => WithReason a -> Set a
withoutReasons = map fst . runWriterT

correct person =
     (withoutReasons $ parentsM person)      == parents person
  && (withoutReasons $ grandparentsM person) == grandparents person

instance Arbitrary Person where
  arbitrary = elements [Inge, Emil, Petra, Willi, Kunibert, Fritz]
