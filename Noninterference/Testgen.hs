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


instance  Arbitrary (Component Parameter) where
  arbitrary = do
      return $ Component {
         input  = S.map Input  $ S.fromList allValues,
         output = S.map Output $ S.fromList allValues
       }

instance  Arbitrary (Implementation Parameter) where
  arbitrary = do
      as <- sublistOf allValues
      bs <- sublistOf allValues
      cs <- sublistOf allValues
      let influences = influencesFrom as bs cs
      return $ Implementation {
         influences = influences
       }
   where
    sublistOf xs = filterM (\_ -> choose (False, True)) xs
    influencesFrom as bs cs = influences
      where influences (Input A) = S.fromList $ fmap Output as
            influences (Input B) = S.fromList $ fmap Output bs
            influences (Input C) = S.fromList $ fmap Output cs
            influences _         = S.empty



instance (Enumerable d, Ord d) => Arbitrary (Specification Parameter d) where
  arbitrary = do
      as <- sublistOf allValues
      bs <- sublistOf allValues
      cs <- sublistOf allValues
      xs <- sublistOf allValues
      ys <- sublistOf allValues
      zs <- sublistOf allValues
      let includes = includesFrom as bs cs xs ys zs
      return $ Specification {
         includes = includes,
         datasets = S.fromList allValues
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


fooSpec :: Specification Parameter Datasets
fooSpec = Specification {
      includes = includes,
      datasets = S.fromList allValues
    }
  where
    includes (Input A) = S.fromList $ []
    includes (Input B) = S.fromList $ [Customer, Provider]
    includes (Input C) = S.fromList $ [Appliance]

    includes (Output X) = S.fromList $ [Customer]
    includes (Output Y) = S.fromList $ [Customer, Appliance]
    includes (Output Z) = S.fromList $ []


fooImpl :: Implementation Parameter
fooImpl = Implementation {
      influences = influences
    }
  where
    influences (Input A) = S.fromList $ fmap Output [X]
    influences (Input B) = S.fromList $ fmap Output [Y,Z]
    influences (Input C) = S.fromList $ fmap Output [Y]

    influences _         = S.empty

foo :: Component Parameter
foo = Component {
      input  = S.map Input  $ S.fromList allValues,
      output = S.map Output $ S.fromList allValues
    }



barSpec :: Specification Parameter Datasets
barSpec = Specification {
      datasets = S.fromList allValues,
      includes = includes
    }
  where
    includes (Input A) = S.fromList $ []
    includes (Input B) = S.fromList $ [Customer, Provider]
    includes (Input C) = S.fromList $ [Appliance]

    includes (Output X) = S.fromList $ [Customer]
    includes (Output Y) = S.fromList $ [Customer, Appliance, Provider]
    includes (Output Z) = S.fromList $ []


barImpl :: Implementation Parameter
barImpl = Implementation {
      influences = influences
    }
  where
    influences (Input A) = S.fromList $ fmap Output [X]
    influences (Input B) = S.fromList $ fmap Output [Y,Z]
    influences (Input C) = S.fromList $ fmap Output [Y]

    influences _         = S.empty

bar :: Component Parameter
bar = Component {
      input  = S.map Input  $ S.fromList allValues,
      output = S.map Output $ S.fromList allValues
    }


exampleSpec = Specification {
                   datasets = S.fromList allValues,
                   includes  = (M.!) $ M.fromList [(Input A,fromList [Customer,Appliance]),
                                                     (Input B,fromList [Customer]),
                                                     (Input C,fromList [Customer,Appliance]),
                                                     (Output X,fromList [Customer,Provider]),
                                                     (Output Y,fromList [Customer]),
                                                     (Output Z,fromList [Customer,Provider,Appliance])]
                 }
example = Component { input = fromList [Input A,Input B,Input C],
                      output = fromList [Output X,Output Y,Output Z]
                 }
exampleImp = Implementation {
                   influences = (M.!) $ M.fromList [(Input A,fromList [Output Y]),
                                                      (Input B,fromList [Output Y,Output Z]),
                                                      (Input C,fromList [Output Y,Output Z]),
                                                      (Output X,fromList []),
                                                      (Output Y,fromList []),
                                                      (Output Z,fromList [])]
                 }


