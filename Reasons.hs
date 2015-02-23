{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


module Reasons where


import Prelude hiding (map)
import Control.Monad.Trans.Writer.Lazy
import Control.Monad(msum)
import Control.Monad.Trans.Class(lift)

import Data.Set.Monad

import Test.QuickCheck
import Data.Typeable


class (Eq (Function r), Ord (Function r), Show (Function r), Typeable (Function r),
       Eq (Relation r), Ord (Relation r), Show (Relation r), Typeable (Relation r)
      ) => Reasons r where
  data Function r
  data Relation r


data Reason r  where
    Axiom1 :: (Typeable s, Show s, Ord s, Reasons r) => Relation r -> s      -> Reason r
    Axiom2 :: (Typeable s, Show s, Ord s,
               Typeable t, Show t, Ord t, Reasons r) => Relation r -> s -> t -> Reason r
    MapsTo :: (Typeable s, Show s, Ord s,
               Typeable t, Show t, Ord t, Reasons r) => Function r -> s -> t -> Reason r
deriving instance Show (Reason r)

instance Eq (Reason r) where
  Axiom1 r x    == Axiom1 r' x'    = r == r' && Just x == cast x'
  Axiom2 r x y  == Axiom2 r' x' y' = r == r' && Just x == cast x' && Just y == cast y'
  _             == _               = False

instance Ord (Reason r) where
  Axiom1 r x    <= Axiom1 r' x'    = r <  r' || (r == r' && ( Just x <= cast x' ))
  Axiom2 r x y  <= Axiom2 r' x' y' = r <  r' || (r == r' && ((Just x <= cast x') || (Just x == cast x') && Just y <= cast y'))
  Axiom1 _ _    <= _               = True
  _             <= Axiom1 _ _      = False
  Axiom2 _ _ _  <= _               = True
  _             <= Axiom2 _ _ _    = False


type WithReason r a = WriterT [Reason r] Set a

because :: [Reason r] -> WithReason r ()
because = tell

withoutReasons :: (Ord a, Reasons r) => WithReason r a -> Set a
withoutReasons = map fst . runWriterT
