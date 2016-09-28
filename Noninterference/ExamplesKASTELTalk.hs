{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
import Unicode
import Noninterference
import Noninterference.Util
import Noninterference.Procedure
import Noninterference.Export

import Data.Set as S
import Data.Set.Unicode
import Data.List
import Data.Char

import Control.Monad(forM_)

import qualified Data.Map as M


data Datasets = Billing
              | Consumption
              | Passwords
              | DeviceStatus
              deriving (Show, Eq, Ord, Enum, Bounded)


data DatasetsGrob = Private
                  | Public
                  deriving (Show, Eq, Ord, Enum, Bounded)


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


instance Enumerable Parameter where
  allValues = [Input i  | i <- allValues] ++
              [Output o | o <- allValues]

noImpl :: Implementation Parameter
noImpl = Implementation {
    influences = \_ -> S.empty
  }

inputs  = S.fromList $ fmap Input  [A,B,C]
outputs = S.fromList $ fmap Output [X,Y,Z]

datasetss :: (Ord d, Enum d, Bounded d) => Set d
datasetss = S.fromList $ allValues

pr = Procedure {
      input  = inputs,
      output = outputs
 }

example1 :: Specification Parameter Datasets
example1 = Specification {
      datasets = datasetss,
      includes = includes
    }
  where
    includes (Input A)   = S.fromList $ [Consumption,Billing]
    includes (Input B)   = S.fromList $ [Passwords]
    includes (Input C)   = S.fromList $ [DeviceStatus]
    includes (Output X)  = S.fromList $ [Billing,Consumption]
    includes (Output Y)  = S.fromList $ [DeviceStatus, Billing]
    includes (Output Z)  = S.fromList $ [Passwords]

example1Weakening :: Specification Parameter Datasets
example1Weakening = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl p@(Input A) = includes example1 p ∪ (S.fromList [Passwords])
    incl p           = includes example1 p


example1Suggestion :: Specification Parameter Datasets
example1Suggestion = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl p
      | p ∈ outputs = includes example1Weakening p ∪ (S.fromList [Passwords])
      | p ∈ inputs  = includes example1Weakening p


example1StrongestGuarantee :: Specification Parameter Datasets
example1StrongestGuarantee = strongestValidGuarantee pr example1 example1Weakening


f Consumption  = S.fromList [Private]
f Billing      = S.fromList [Public]
f DeviceStatus = S.fromList [Private]
f Passwords    = S.fromList [Private]

example1Grob :: Specification Parameter DatasetsGrob
example1Grob = relabeledRevUsing example1 f


example2 :: Specification Parameter Datasets
example2 = Specification {
  datasets = datasetss,
  includes = (M.!) $ M.fromList [
      (Input A,fromList [Passwords,DeviceStatus]),
      (Input B,fromList [Passwords]),
      (Input C,fromList [Passwords,Consumption]),

      (Output X,fromList [Passwords,Billing,DeviceStatus]),
      (Output Y,fromList [Passwords,Consumption,DeviceStatus]),
      (Output Z,fromList [Passwords])]
 }

{-
example2Incl :: Implementation Parameter
example2Incl = Implementation {
  influences = (M.!) $ M.fromList [
      (Input A,fromList [Output X]),
      (Input B,fromList [Output X,Output Y,Output Z]),
      (Input C,fromList []),

      (Output X,fromList []),
      (Output Y,fromList []),
      (Output Z,fromList [])]
  }
-}

example2Grob :: Specification Parameter DatasetsGrob
example2Grob = Specification {
  datasets = datasetss,
  includes = (M.!) $ M.fromList [
      (Input A,fromList [Private]),
      (Input B,fromList [Private]),
      (Input C,fromList [Public,Private]),

      (Output X,fromList [Public,Private]),
      (Output Y,fromList [Public,Private]),
      (Output Z,fromList [Private])]
}

example2GrobImpl :: Implementation Parameter
example2GrobImpl = Implementation {
  influences = (M.!) $ M.fromList [
      (Input A,fromList [Output X]),
      (Input B,fromList [Output X,Output Y,Output Z]),
      (Input C,fromList []),

      (Output X,fromList []),
      (Output Y,fromList []),
      (Output Z,fromList [])
      ]
  }


example2AnotherImpl :: Implementation Parameter
example2AnotherImpl = Implementation {
  influences = (M.!) $ M.fromList [
      (Input A,fromList [Output X]),
      (Input B,fromList [Output X,Output Y,Output Z]),
      (Input C,fromList [Output Y]),

      (Output X,fromList []),
      (Output Y,fromList []),
      (Output Z,fromList [])
      ]
  }
