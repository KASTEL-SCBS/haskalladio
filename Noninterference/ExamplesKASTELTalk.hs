{-# LANGUAGE NamedFieldPuns #-}
import Unicode
import Security
import Noninterference
import Noninterference.Util
import Noninterference.Procedure
import Noninterference.Comparison
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


inputs  = S.fromList $ fmap Input  [A,B,C]
outputs = S.fromList $ fmap Output [X,Y,Z]

example1 :: Procedure Parameter Datasets
example1 = Procedure {
      input  = inputs,
      output = outputs,
      includes = includes,
      influences = influences
    }
  where
    includes (Input A)   = S.fromList $ [Consumption,Billing]
    includes (Input B)   = S.fromList $ [Passwords]
    includes (Input C)   = S.fromList $ [DeviceStatus]
    includes (Output X)  = S.fromList $ [Billing,Consumption]
    includes (Output Y)  = S.fromList $ [DeviceStatus, Billing]
    includes (Output Z)  = S.fromList $ [Passwords]

    influences _         = S.empty

example1Weakening :: Procedure Parameter Datasets
example1Weakening = example1 {
      includes = incl
    }
  where
    incl p@(Input A) = includes example1 p ∪ (S.fromList [Passwords])
    incl p           = includes example1 p


example1Suggestion :: Procedure Parameter Datasets
example1Suggestion = example1 {
      includes = incl
    }
  where
    incl p
      | p ∈ outputs = includes example1Weakening p ∪ (S.fromList [Passwords])
      | p ∈ inputs  = includes example1Weakening p


example1StrongestGuarantee :: Procedure Parameter Datasets
example1StrongestGuarantee = strongestValidGuarantee example1 example1Weakening




f Consumption  = S.fromList [Private]
f Billing      = S.fromList [Public]
f DeviceStatus = S.fromList [Private]
f Passwords    = S.fromList [Private]

example1Grob :: Procedure Parameter DatasetsGrob
example1Grob = relabeledRevUsing example1 f


example2 :: Procedure Parameter Datasets
example2 =  Procedure {
  input  = fromList [Input A,Input B,Input C],
  output = fromList [Output X,Output Y,Output Z],
  includes = (M.!) $ M.fromList [
      (Input A,fromList [DeviceStatus]),
      (Input B,fromList []),
      (Input C,fromList [Consumption]),

      (Output X,fromList [Billing,DeviceStatus]),
      (Output Y,fromList [Consumption,DeviceStatus]),
      (Output Z,fromList [])],

  influences = \_ -> S.empty
  -- influences = (M.!) $ M.fromList [
  --     (Input A,fromList [Output X]),
  --     (Input B,fromList [Output X,Output Y,Output Z]),
  --     (Input C,fromList []),

  --     (Output X,fromList []),
  --     (Output Y,fromList []),
  --     (Output Z,fromList [])]
  }

example2Grob :: Procedure Parameter DatasetsGrob
example2Grob = Procedure {
  input = fromList [Input A,Input B,Input C],
  output = fromList [Output X,Output Y,Output Z],
  includes = (M.!) $ M.fromList [
      (Input A,fromList [Private]),
      (Input B,fromList [Private]),
      (Input C,fromList [Public,Private]),

      (Output X,fromList [Public,Private]),
      (Output Y,fromList [Public,Private]),
      (Output Z,fromList [Private])],
  influences = \_ -> S.empty
  -- influences = (M.!) $ M.fromList [
  --     (Input A,fromList [Output X]),
  --     (Input B,fromList [Output X,Output Y,Output Z]),
  --     (Input C,fromList []),

  --     (Output X,fromList []),
  --     (Output Y,fromList []),
  --     (Output Z,fromList [])
  --     ]
  }
