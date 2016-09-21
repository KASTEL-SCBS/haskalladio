-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Noninterference.Testgen where

import Noninterference.Procedure
import Noninterference.Util

import Algebra.Lattice
import Unicode

import Data.Set as S
import Data.Set.Unicode
import Data.Maybe (fromJust)

import Control.Monad(filterM)
import Test.QuickCheck hiding (output)

import qualified Data.Map as M


data Input  = A
            | B
            | C
            deriving (Show, Eq, Ord, Enum, Bounded)
data Output = X
            | Y
            | Z
            deriving (Show, Eq, Ord, Enum, Bounded)

data Parameter = Input Input
               | Output Output
              deriving (Show, Eq, Ord)

instance Enum Parameter where
  toEnum i
    | i >= 0    = Input  $ toEnum i
    | otherwise = Output $ toEnum (- i -1)
  fromEnum (Input x)  = fromEnum x
  fromEnum (Output x) = (- fromEnum x) - 1
  enumFrom x = enumFromTo x maxBound

instance Bounded Parameter where
  minBound = Output (maxBound)
  maxBound = Input (maxBound)


data Datasets = Customer
              | Provider
              | Appliance
              deriving (Show, Eq, Ord, Enum, Bounded)


instance (Enumerable d, Ord d) => Arbitrary (Procedure Parameter d) where
  arbitrary = do
      as <- sublistOf allValues
      bs <- sublistOf allValues
      cs <- sublistOf allValues
      xs <- sublistOf allValues
      ys <- sublistOf allValues
      zs <- sublistOf allValues
      let includes = includesFrom as bs cs xs ys zs
      as <- sublistOf allValues
      bs <- sublistOf allValues
      cs <- sublistOf allValues
      let influences = influencesFrom as bs cs
      return $ Procedure {
         input  = S.map Input  $ S.fromList allValues,
         output = S.map Output $ S.fromList allValues,
         includes = includes,
         influences = influences
       }
   where
    sublistOf xs = filterM (\_ -> choose (False, True)) xs
    includesFrom as bs cs xs ys zs = includes
      where includes (Input A) = S.fromList $ as
            includes (Input B) = S.fromList $ bs
            includes (Input C) = S.fromList $ cs
            includes (Output X) = S.fromList $ xs
            includes (Output Y) = S.fromList $ ys
            includes (Output Z) = S.fromList $ zs
    influencesFrom as bs cs = influences
      where influences (Input A) = S.fromList $ fmap Output as
            influences (Input B) = S.fromList $ fmap Output bs
            influences (Input C) = S.fromList $ fmap Output cs
            influences _         = S.empty

-- A SpecificationPar consists of two different specfications `includes` for the same implementation `influences`
data SpecificationPair p d d' = SpecificationPair (Procedure p d) (Procedure p d') deriving Show
instance (Enumerable d, Ord d, Enumerable d', Ord d') => Arbitrary (SpecificationPair Parameter d d') where
  arbitrary = do
      pr  <- arbitrary
      pr' <- arbitrary
      return $ SpecificationPair pr (pr { includes = includes pr' })


data SpecificationTriplet p d d' d'' = SpecificationTriplet (Procedure p d) (Procedure p d') (Procedure p d'') deriving Show
instance (Enumerable d, Ord d, Enumerable d', Ord d', Enumerable d'', Ord d'') => Arbitrary (SpecificationTriplet Parameter d d' d'') where
  arbitrary = do
      pr   <- arbitrary
      pr'  <- arbitrary
      pr'' <- arbitrary
      return $ SpecificationTriplet pr (pr { includes = includes pr' }) (pr { includes = includes pr'' })


foo :: Procedure Parameter Datasets
foo = Procedure {
      input  = S.map Input  $ S.fromList allValues,
      output = S.map Output $ S.fromList allValues,
      includes = includes,
      influences = influences
    }
  where
    includes (Input A) = S.fromList $ []
    includes (Input B) = S.fromList $ [Customer, Provider]
    includes (Input C) = S.fromList $ [Appliance]

    includes (Output X) = S.fromList $ [Customer]
    includes (Output Y) = S.fromList $ [Customer, Appliance]
    includes (Output Z) = S.fromList $ []

    influences (Input A) = S.fromList $ fmap Output [X]
    influences (Input B) = S.fromList $ fmap Output [Y,Z]
    influences (Input C) = S.fromList $ fmap Output [Y]

    influences _         = S.empty


bar :: Procedure Parameter Datasets
bar = Procedure {
      input  = S.map Input  $ S.fromList allValues,
      output = S.map Output $ S.fromList allValues,
      includes = includes,
      influences = influences
    }
  where
    includes (Input A) = S.fromList $ []
    includes (Input B) = S.fromList $ [Customer, Provider]
    includes (Input C) = S.fromList $ [Appliance]

    includes (Output X) = S.fromList $ [Customer]
    includes (Output Y) = S.fromList $ [Customer, Appliance, Provider]
    includes (Output Z) = S.fromList $ []

    influences (Input A) = S.fromList $ fmap Output [X]
    influences (Input B) = S.fromList $ fmap Output [Y,Z]
    influences (Input C) = S.fromList $ fmap Output [Y]

    influences _         = S.empty


example = Procedure { input = fromList [Input A,Input B,Input C],
                   output = fromList [Output X,Output Y,Output Z],
                   includes  = (M.!) $ M.fromList [(Input A,fromList [Customer,Appliance]),
                                                     (Input B,fromList [Customer]),
                                                     (Input C,fromList [Customer,Appliance]),
                                                     (Output X,fromList [Customer,Provider]),
                                                     (Output Y,fromList [Customer]),
                                                     (Output Z,fromList [Customer,Provider,Appliance])],
                  influences = (M.!) $ M.fromList [(Input A,fromList [Output Y]),
                                                      (Input B,fromList [Output Y,Output Z]),
                                                      (Input C,fromList [Output Y,Output Z]),
                                                      (Output X,fromList []),
                                                      (Output Y,fromList []),
                                                      (Output Z,fromList [])]
                 }


