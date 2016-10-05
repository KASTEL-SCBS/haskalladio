{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
import Unicode
import Noninterference
import Noninterference.Util
import Noninterference.Procedure
import Noninterference.Export


import Noninterference.Testgen hiding (Datasets)
import Test.QuickCheck hiding (output)


import Data.Set as S
import Data.Set.Unicode
import Data.List
import Data.Char

import Control.Monad(forM_)

import qualified Data.Map as M


data Datasets = Billing
              | BillingSO
              | BillingSOQM
              | Consumption
              | Passwords
              | DeviceStatus
              | PasswordsQuestionMark
              deriving (Eq, Ord, Enum, Bounded)

instance Show Datasets where
  show Billing = "Billing"
  show Consumption = "Consumption"
  show Passwords = "Passwords"
  show DeviceStatus = "DeviceStatus"
  show PasswordsQuestionMark = "Passwords??"
  show BillingSOQM  = "\\sout{Billing} ??"
  show BillingSO    = "\\sout{Billing}"


data DatasetsGrob = Home
                  | Money
                  deriving (Show, Eq, Ord, Enum, Bounded)


-- data Input  = A
--             | B
--             | C
--             deriving (Show, Eq, Ord, Enum, Bounded)
-- data Output = X
--             | Y
--             | Z
--             deriving (Show, Eq, Ord, Enum, Bounded)

-- data Parameter = Input Input
--                | Output Output
--               deriving ( Eq, Ord)

-- instance Show Parameter where
--   show (Input i)  = show i
--   show (Output o) = show o


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

pr = Component {
      input  = inputs,
      output = outputs
 }

example1Impl :: Implementation Parameter
example1Impl = Implementation {
      influences = influences
    }
  where influences (Input A) = S.fromList [Output X]
        influences (Input B) = S.fromList [Output Z]
        influences (Input C) = S.fromList []


example1ImplViol :: Implementation Parameter
example1ImplViol = Implementation {
      influences = influences
    }
  where influences (Input A) = S.fromList [Output X, Output Y]
        influences (Input B) = S.fromList [Output Z]
        influences (Input C) = S.fromList []

example1 :: Specification Parameter Datasets
example1 = Specification {
      datasets = datasetss,
      includes = includes
    }
  where
    includes (Input A)   = S.fromList $ [Consumption,Billing]
    includes (Input B)   = S.fromList $ [Passwords]
    includes (Input C)   = S.fromList $ [Billing,DeviceStatus]
    includes (Output X)  = S.fromList $ [Billing,Consumption]
    includes (Output Y)  = S.fromList $ [DeviceStatus, Billing]
    includes (Output Z)  = S.fromList $ [Passwords]

example1Strong :: Specification Parameter Datasets
example1Strong = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl p@(Input A) = includes example1 p ∖ (S.fromList [Billing])
    incl p           = includes example1 p


example1StrongQM :: Specification Parameter Datasets
example1StrongQM = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl p@(Input A)  = (includes example1 p ∖ (S.fromList [Billing])) ∪ S.fromList [BillingSO]
    incl p@(Output X) = (includes example1 p ∖ (S.fromList [Billing])) ∪ S.fromList [BillingSOQM]
    incl p@(Output Y) = (includes example1 p ∖ (S.fromList [Billing])) ∪ S.fromList [BillingSOQM]
    incl p            = includes example1 p


example1Weakening :: Specification Parameter Datasets
example1Weakening = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl p@(Input A) = includes example1 p ∪ (S.fromList [Passwords])
    incl p           = includes example1 p


example1WeakeningQM :: Specification Parameter Datasets
example1WeakeningQM = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl p@(Input A)  = includes example1 p ∪ (S.fromList [Passwords])
    incl p@(Output X) = includes example1 p ∪ (S.fromList [PasswordsQuestionMark])
    incl p@(Output Y) = includes example1 p ∪ (S.fromList [PasswordsQuestionMark])
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


f Consumption  = S.fromList [Home]
f Billing      = S.fromList [Money]
f DeviceStatus = S.fromList [Home]
f Passwords    = S.fromList [Home]

example1Grob :: Specification Parameter DatasetsGrob
example1Grob = relabeledRevUsing example1 f


example2 :: Specification Parameter Datasets
example2 = Specification {
  datasets = datasetss,
  includes = (M.!) $ M.fromList [
      (Input A,fromList [DeviceStatus]),
      (Input B,fromList []),
      (Input C,fromList [Consumption]),

      (Output X,fromList [Billing,DeviceStatus]),
      (Output Y,fromList [Consumption,DeviceStatus]),
      (Output Z,fromList [])]
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
      (Input A,fromList [Home]),
      (Input B,fromList [Home]),
      (Input C,fromList [Money,Home]),

      (Output X,fromList [Money,Home]),
      (Output Y,fromList [Money,Home]),
      (Output Z,fromList [Home])]
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


example3 = Specification { includes = (M.!) $ M.fromList [(Input A,fromList []),(Input B,fromList [Billing,Consumption,DeviceStatus]),(Input C,fromList [DeviceStatus]),(Output X,fromList [Billing,Consumption,Passwords]),(Output Y,fromList [Consumption,DeviceStatus]),(Output Z,fromList [Passwords])], datasets = fromList [Billing,Consumption,Passwords,DeviceStatus] }
example3Grob = Specification { includes = (M.!) $ M.fromList [(Input A,fromList [Money]),(Input B,fromList [Money]),(Input C,fromList [Money]),(Output X,fromList [Home,Money]),(Output Y,fromList [Home,Money]),(Output Z,fromList [Home,Money])], datasets = fromList [Home,Money] }


small :: Component Parameter
small = Component {
      input  = S.fromList $ fmap Input  [A],
      output = S.fromList $ fmap Output [X]
 }

smallImpl :: Implementation Parameter
smallImpl =  Implementation {
      influences = influences
    }
  where influences (Input A) = S.fromList [Output X]
        influences _         = S.fromList []

smallSpec1 :: Specification Parameter Datasets
smallSpec1 = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl (Input A)  = S.fromList []
    incl (Output X) = S.fromList [Passwords]
    incl _          = S.fromList []

smallSpec2 :: Specification Parameter Datasets
smallSpec2 = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl (Input A)  = S.fromList [Passwords]
    incl (Output X) = S.fromList []
    incl _          = S.fromList []

main = if (    (not $ isConsistentRelabelingRevFor pr f example1 example1Grob)
           ||  (existsConsistentRelabelingRevFor pr example2 example2Grob)
           ||  (existsConsistentRelabelingRevFor pr example3 example3Grob)
           ||  (not $ secure joana pr example1Impl example1)
           ||  (not $ secure joana pr smallImpl smallSpec1)
          ) then error "rofl" else do
       forM_ [
              ("exampleOne",                    False, False, pr, noImpl,            example1),
              ("exampleOneImpl",                False, False, pr, example1Impl,      example1),
              ("exampleOneImplWithSpec",        True,  False, pr, example1Impl,      example1),
              ("exampleOneImplWithSpecViol",    True,  False, pr, example1ImplViol,  example1),
              ("smallSpecOne",                  True,  False, small, smallImpl, smallSpec1),
              ("smallSpecTwo",                  True,  False, small, smallImpl, smallSpec2),

              ("exampleOneWithSpec",            True, True,  pr, noImpl,            example1),
              ("exampleOneGammaWithSpec",       True, False, pr, γ pr example1,     example1),
              ("exampleOneWithSpecWeakQM",      True, True,  pr, noImpl,            example1WeakeningQM),
              ("exampleOneGammaWithSpecWeakQM", True, False, pr, γ pr example1,     example1WeakeningQM),
--              ("exampleOneWithSpecWeakMono",    True, True,  pr, noImpl,            strongestValidGuaranteeExtensive pr example1 example1Weakening),
--              ("exampleOneGammaWithSpecWeakMono",True,False, pr, γ pr example1,     strongestValidGuaranteeExtensive pr example1 example1Weakening),
              ("exampleOneWithSpecWeak",        True, True,  pr, noImpl,            strongestValidGuarantee pr example1 example1Weakening),
              ("exampleOneGammaWithSpecWeak",   True, False,  pr, γ pr example1,     strongestValidGuarantee pr example1 example1Weakening),

              ("exampleOneGammaWithSpecStrongQM", True, False,  pr, γ pr example1,   example1StrongQM),
              ("exampleOneGammaWithSpecStrong",   True, False,  pr, γ pr example1,   strongestValidGuarantee pr example1 example1Strong),

              ("exampleTwoWithSpec",            True, True,  pr, noImpl,            example2),
              ("exampleTwoGammaWithSpec",       True, False, pr, γ pr example2,     example2),
              ("exampleThreeWithSpec",          True, True,  pr, noImpl,            example3)
             ]
        (\(name,showSpec, questionmark,pr,impl,sp) -> putStrLn $ toTikzNamed name showSpec questionmark pr impl sp)
       forM_ [
              ("exampleOneGrobRelabeled",    True, True,  pr, noImpl,            example1Grob),
              ("exampleOneGammaGrob",        True, False, pr, γ pr example1Grob, example1Grob),
              ("exampleTwoWithSpecGrob",     True, True,  pr, noImpl,            example2Grob),
              ("exampleTwoGammaWithSpecGrob",True, False, pr, γ pr example2Grob, example2Grob),
              ("exampleThreeWithSpecGrob",   True, True,  pr, noImpl,            example3Grob)
             ]
        (\(name,showSpec, questionmark,pr,impl,sp) -> putStrLn $ toTikzNamed name showSpec questionmark pr impl sp)

