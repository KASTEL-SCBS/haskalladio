{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
import Unicode
import Noninterference
import Noninterference.Util
import Noninterference.Procedure
import Noninterference.Export


-- import Noninterference.Testgen hiding (Datasets)
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


data Parameter =
              A
            | B
            | C
            | X
            | Y
            | Z
            | N
            | M
            deriving (Eq, Ord, Enum, Bounded, Show)


-- instance Enumerable Parameter where
--   allValues = [i  | i <- allValues] ++
--               [o | o <- allValues]

noImpl :: Implementation Parameter
noImpl = Implementation {
    influences = \_ -> S.empty
  }


inputs  = S.fromList [A,B,C]
outputs = S.fromList [X,Y,Z]

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
  where influences (A) = S.fromList [X]
        influences (B) = S.fromList [Z]
        influences (C) = S.fromList []


example1ImplViol :: Implementation Parameter
example1ImplViol = Implementation {
      influences = influences
    }
  where influences (A) = S.fromList [X, Y]
        influences (B) = S.fromList [Z]
        influences (C) = S.fromList []

example1 :: Specification Parameter Datasets
example1 = Specification {
      datasets = datasetss,
      includes = includes
    }
  where
    includes (A)   = S.fromList $ [Consumption,Billing]
    includes (B)   = S.fromList $ [Passwords]
    includes (C)   = S.fromList $ [Billing,DeviceStatus]
    includes (X)  = S.fromList $ [Billing,Consumption]
    includes (Y)  = S.fromList $ [DeviceStatus, Billing]
    includes (Z)  = S.fromList $ [Passwords]

example1Strong :: Specification Parameter Datasets
example1Strong = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl p@(A) = includes example1 p ∖ (S.fromList [Billing])
    incl p           = includes example1 p


example1StrongQM :: Specification Parameter Datasets
example1StrongQM = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl p@(A)  = (includes example1 p ∖ (S.fromList [Billing])) ∪ S.fromList [BillingSO]
    incl p@(X) = (includes example1 p ∖ (S.fromList [Billing])) ∪ S.fromList [BillingSOQM]
    incl p@(Y) = (includes example1 p ∖ (S.fromList [Billing])) ∪ S.fromList [BillingSOQM]
    incl p            = includes example1 p


example1Weakening :: Specification Parameter Datasets
example1Weakening = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl p@(A) = includes example1 p ∪ (S.fromList [Passwords])
    incl p           = includes example1 p


example1WeakeningQM :: Specification Parameter Datasets
example1WeakeningQM = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl p@(A)  = includes example1 p ∪ (S.fromList [Passwords])
    incl p@(X) = includes example1 p ∪ (S.fromList [PasswordsQuestionMark])
    incl p@(Y) = includes example1 p ∪ (S.fromList [PasswordsQuestionMark])
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
      (A,fromList [DeviceStatus]),
      (B,fromList []),
      (C,fromList [Consumption]),

      (X,fromList [Billing,DeviceStatus]),
      (Y,fromList [Consumption,DeviceStatus]),
      (Z,fromList [])]
 }

{-
example2Incl :: Implementation Parameter
example2Incl = Implementation {
  influences = (M.!) $ M.fromList [
      (A,fromList [X]),
      (B,fromList [X,Y,Z]),
      (C,fromList []),

      (X,fromList []),
      (Y,fromList []),
      (Z,fromList [])]
  }
-}

example2Grob :: Specification Parameter DatasetsGrob
example2Grob = Specification {
  datasets = datasetss,
  includes = (M.!) $ M.fromList [
      (A,fromList [Home]),
      (B,fromList [Home]),
      (C,fromList [Money,Home]),

      (X,fromList [Money,Home]),
      (Y,fromList [Money,Home]),
      (Z,fromList [Home])]
}

example2GrobImpl :: Implementation Parameter
example2GrobImpl = Implementation {
  influences = (M.!) $ M.fromList [
      (A,fromList [X]),
      (B,fromList [X,Y,Z]),
      (C,fromList []),

      (X,fromList []),
      (Y,fromList []),
      (Z,fromList [])
      ]
  }


example2AnotherImpl :: Implementation Parameter
example2AnotherImpl = Implementation {
  influences = (M.!) $ M.fromList [
      (A,fromList [X]),
      (B,fromList [X,Y,Z]),
      (C,fromList [Y]),

      (X,fromList []),
      (Y,fromList []),
      (Z,fromList [])
      ]
  }


example3 = Specification { includes = (M.!) $ M.fromList [(A,fromList []),(B,fromList [Billing,Consumption,DeviceStatus]),(C,fromList [DeviceStatus]),(X,fromList [Billing,Consumption,Passwords]),(Y,fromList [Consumption,DeviceStatus]),(Z,fromList [Passwords])], datasets = fromList [Billing,Consumption,Passwords,DeviceStatus] }
example3Grob = Specification { includes = (M.!) $ M.fromList [(A,fromList [Money]),(B,fromList [Money]),(C,fromList [Money]),(X,fromList [Home,Money]),(Y,fromList [Home,Money]),(Z,fromList [Home,Money])], datasets = fromList [Home,Money] }


small :: Component Parameter
small = Component {
      input  = S.fromList [A],
      output = S.fromList [X]
 }

smallImpl :: Implementation Parameter
smallImpl =  Implementation {
      influences = influences
    }
  where influences (A) = S.fromList [X]
        influences _         = S.fromList []

smallSpec1 :: Specification Parameter Datasets
smallSpec1 = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl (A)  = S.fromList []
    incl (X) = S.fromList [Passwords]
    incl _          = S.fromList []

smallSpec2 :: Specification Parameter Datasets
smallSpec2 = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl (A)  = S.fromList [Passwords]
    incl (X) = S.fromList []
    incl _          = S.fromList []



prLeft = Component {
      input  = S.fromList [A, B],
      output = S.fromList [X, Y]
    }

prRight = Component {
      input  = S.fromList [X, Y],
      output = S.fromList [N, M]
    }

spLeft ::  Specification Parameter DatasetsGrob
spLeft = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl (A)  = S.fromList [Home]
    incl (B)  = S.fromList [Money]
    incl (X)  = S.fromList [Home, Money]
    incl (Y)  = S.fromList [Home, Money]


spLeft' ::  Specification Parameter DatasetsGrob
spLeft' = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl (A)  = S.fromList [Home]
    incl (B)  = S.fromList [Money]
    incl (X)  = S.fromList [Home]
    incl (Y)  = S.fromList [Money]


spRightFine ::  Specification Parameter Datasets
spRightFine = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl (X)  = S.fromList [Consumption]
    incl (Y)  = S.fromList [Billing]
    incl (N)  = S.fromList [Consumption]
    incl (M)  = S.fromList [Billing]


spRight ::  Specification Parameter DatasetsGrob
spRight = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl (X)  = S.fromList [Home, Money]
    incl (Y)  = S.fromList [Home, Money]
    incl (N)  = S.fromList [Home, Money]
    incl (M)  = S.fromList [Home, Money]



spRight' ::  Specification Parameter DatasetsGrob
spRight' = Specification {
      datasets = datasetss,
      includes = incl
    }
  where
    incl (X)  = S.fromList [Home]
    incl (Y)  = S.fromList [Money]
    incl (N)  = undefined
    incl (M)  = undefined




main = if (    (not $ isConsistentRelabelingRevFor pr f example1 example1Grob)
           ||  (existsConsistentRelabelingRevFor pr example2 example2Grob)
           ||  (existsConsistentRelabelingRevFor pr example3 example3Grob)
           ||  (not $ secure joana pr example1Impl example1)
           ||  (not $ secure joana pr smallImpl smallSpec1)
           ||  (not $ secure joana prRight (γ prRight spRightFine) spRight)
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
              ("exampleThreeWithSpec",          True, True,  pr, noImpl,            example3),

              ("spRightFine",                   True, False, prRight, (γ prRight spRightFine),        spRightFine)
             ]
        (\(name,showSpec, questionmark,pr,impl,sp) -> putStrLn $ toTikzNamed name showSpec questionmark pr impl sp)
       forM_ [
              ("exampleOneGrobRelabeled",    True, True,  pr, noImpl,            example1Grob),
              ("exampleOneGammaGrob",        True, False, pr, γ pr example1Grob, example1Grob),
              ("exampleTwoWithSpecGrob",     True, True,  pr, noImpl,            example2Grob),
              ("exampleTwoGammaWithSpecGrob",True, False, pr, γ pr example2Grob, example2Grob),
              ("exampleThreeWithSpecGrob",   True, True,  pr, noImpl,            example3Grob),

              ("spLeft",                        True, True,  prLeft, noImpl,        spLeft),
              ("spLeftAssumed",                 True, True,  prLeft, noImpl,        spLeft'),
              ("spRight",                       True, False, prRight, (γ prRight spRightFine),        spRight),
              ("spRightThen",                   True, False, prRight, (γ prRight spRightFine),        (strongestValidGuarantee prRight spRightFine spRight' :: Specification Parameter DatasetsGrob))
             ]
        (\(name,showSpec, questionmark,pr,impl,sp) -> putStrLn $ toTikzNamed name showSpec questionmark pr impl sp)

