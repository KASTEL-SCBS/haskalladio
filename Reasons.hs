{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}



module Reasons where


import Prelude hiding (map)
import Control.Monad.Trans.Writer.Lazy
import Control.Monad(msum, guard)
import Control.Monad.Trans.Class(lift)

import Data.Set.Monad

import Test.QuickCheck
import Data.Typeable


import Misc

type ReasonLike r = (Eq r, Ord r, Show r, Typeable r)

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
    Not    :: Reason r -> Reason r
deriving instance Show (Reason r)

instance Eq (Reason r) where
  Axiom1 r x    == Axiom1 r' x'    = r == r' && Just x == cast x'
  Axiom2 r x y  == Axiom2 r' x' y' = r == r' && Just x == cast x' && Just y == cast y'
  _             == _               = False

instance Ord (Reason r) where
  Axiom1 r x    <= Axiom1 r' x'    = r <  r' || (r == r' && ( Just x <= cast x' ))
  Axiom2 r x y  <= Axiom2 r' x' y' = r <  r' || (r == r' && ((Just x <= cast x') || (Just x == cast x') && Just y <= cast y'))
  MapsTo f x y  <= MapsTo f' x' y' = f <  f' || (f == f' && ((Just x <= cast x') || (Just x == cast x') && Just y <= cast y'))
  Not r         <= Not r'          = r <= r'

  Axiom1 _ _    <= _               = True
  _             <= Axiom1 _ _      = False
  Axiom2 _ _ _  <= _               = True
  _             <= Axiom2 _ _ _    = False
  MapsTo _ _ _  <= _               = True
  _             <= MapsTo _ _ _    = False
  Not _         <= _               = True
  _             <= Not _           = False

type WithReason r a = WriterT [Reason r] Set a

because :: [Reason r] -> WithReason r ()
because = tell

neg :: (Ord a) => Reason r -> WithReason r a -> WithReason r ()
neg r a = do
  guard $ isEmpty successes
  because $ [Not r]
  return ()
 where successes  = runWriterT a

notM :: Reason r -> Bool ->  WithReason r ()
notM r b = do
  guard $ not b
  because $ [Not r]
  return ()

withoutReasons :: (Ord a, Reasons r) => WithReason r a -> Set a
withoutReasons = map fst . runWriterT

liftR2 :: (Reasons r, ReasonLike a, ReasonLike b) => Relation r -> (a -> Set b) -> (a -> WithReason r b)
liftR2 r f a = do
   b <- lift $ f a
   because $ [Axiom2 r a b]
   return b

liftNot2 :: (Reasons r, ReasonLike a, ReasonLike b) => Relation r -> (a -> Set b) -> (a -> b -> WithReason r ())
liftNot2 r f a b = notM (Axiom2 r a b)
                        (b ∈ f a )

liftF :: (Reasons r, ReasonLike a, ReasonLike b) => Function r -> (a -> b) -> (a -> WithReason r b)
liftF r f a = do
   because $ [MapsTo r a y]
   return y
  where y = f a
