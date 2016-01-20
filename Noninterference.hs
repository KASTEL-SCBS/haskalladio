{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Noninterference where

import Noninterference.Util
import Noninterference.Procedure
import Noninterference.Testgen

import Prelude as P

import Algebra.Lattice
import Unicode

import Data.Set as S
import Data.Set.Unicode
import Data.Maybe (fromJust)

import qualified Data.Map as M

import Control.Monad.Writer
import Control.Monad

import Test.QuickCheck hiding (output)

type OrderedSet p = (Set p, p -> p -> Bool)

-- The standard low/high lattice
data LowHigh = Low
             | High
             deriving (Show, Eq, Ord, Enum, Bounded)

lowhigh :: OrderedSet LowHigh
lowhigh = (S.fromList [Low, High], (⊑))
  where Low  ⊑ Low  = True
        Low  ⊑ High = True
        High ⊑ High = True
        High ⊑ Low  = False



-- An verification condition (derived from a ifc specification) is
-- a "classical" non-interference condition, and hence consists of
-- * a lattice structure on on the set l of security levels
-- * a mapping from parameters to their security level in l
type VerificationCondition p l = (OrderedSet l, p -> l)

-- A SpecificationInterpretation gives a ifc specification meaning by
-- deriving a number of verification conditions that all have to hold
-- for the ifc specification to hold.
type SpecificationInterpretation p d l = Procedure p d -> [VerificationCondition p l]


-- Simon's approach for KeY is to translate the ifc-specification into several non-interference verification
-- condition, each for the LowHigh lattice.
greiner :: (Ord d, Ord p) => SpecificationInterpretation p d LowHigh
greiner pr@(Procedure { input, output, includes, influences}) =
    [ (lowhigh, (\p -> if (d ∈ includes p) then High else Low)) | d <- toList $ datasets pr]

-- Martin's Approach for JOANA generates just one JOANA-Specification, using the powerset-lattice of d.
hecker :: (Ord d, Ord p) => SpecificationInterpretation p d (Set d)
hecker pr@(Procedure { input, output, includes, influences}) = [((powerset (datasets pr), (⊆)), includes )]

-- In this simplified model, a procedures implementation is abstractly defined by it's  information-flow
-- between input and output variables, as defined by the function "influences".
-- Hence, a verification condition holds if this flow does not exceed that allowed by the flow lattice:
holds :: (Ord p) => Procedure p d -> VerificationCondition p l -> Bool
holds (Procedure { input, output, includes, influences}) ((l,(⊑)), classifiedAs) =
    (∀) input (\(i :: p) ->   (∀) output (\(o :: p) ->
          (o ∈ (influences i)) → (classifiedAs i ⊑ classifiedAs o)
    ))

-- Given an ifc-specification interpretation (e.g. "hecker" or "greiner"),
-- a procedure pr is secure iff all its verification conditions hold.
secure interpretation pr = and [ holds pr condition | condition <- interpretation pr ]

-- Lemma 1.: Hecker and Greiner are equivalent! Otherwise, we couldn't use KeY and JOANA interchangably!!
heckerIsGreiner :: (Ord d, Ord p) => Procedure p d -> Bool
heckerIsGreiner pr = secure hecker pr ↔ secure greiner pr


-- Before we can we define a criterion which allows us to re-use existing non-interference proofs by employing *relabelings*,
-- we introduce the concept of "weak / strong" ifc-specifications:

-- given two different ifc requirements for the same procedure using the same datasets,
-- i.e.: given procedures pr, pr' such that
--   * input  pr == input  pr'
--   * output pr == output pr'
--   * influences pr == influences pr'
-- , the ifc requirement of pr is called "naively stronger" than that of pr' iff
--      pr `isNaivelyStrongerThan` pr'
-- as defined here:
isNaivelyStrongerThan ::  (Ord d) => Procedure p d ->  Procedure p d -> Bool
pr `isNaivelyStrongerThan` pr'  =
      and [ includes pr' p ⊆ includes pr p | p <- toList $ input pr]
  &&  and [ includes pr' p ⊇ includes pr p | p <- toList $ output pr]


-- Some properties concerning weakenings:
--
-- Lemma 2.: weakenings of secure ifc-specifications are secure.
secureWeakeningsAreSecure :: (Enum d, Bounded d, Ord d, Ord p) => Procedure p d -> Procedure p d -> Property
secureWeakeningsAreSecure pr pr' =
  (secure hecker pr) && (pr `isNaivelyStrongerThan` pr')
  ==>
  (secure hecker pr')
-- unfortunately, naively checking this using QuickCheck is inefficient.

-- Instead, we define the enumeration of all "weakenings" of a given ifc specification:
-- "weakings pr" enumerates all weakenings of pr, i.e. all procedures pr' such that
--      pr `isNaivelyStrongerThan` pr'
weakenings :: (Ord d, Ord p, Enum d, Bounded d) => Procedure p d -> [Procedure p d]
weakenings pr@(Procedure { input, output, includes, influences}) =
   [ pr { includes = \p -> fromJust $ lookup p choice } | choice <- choices ]
  where choices = chooseOneEach $    [(i, [d | d <- toList $ powerset $ fromList allValues, d ⊆ includes i]) | i <- toList $  input]
                                  ++ [(o, [d | d <- toList $ powerset $ fromList allValues, d ⊇ includes o]) | o <- toList $ output]

        chooseOneEach :: [(a,[b])] -> [[(a,b)]]
        chooseOneEach choices = fmap (zip as) $ sequence bss
          where as  = fmap fst choices
                bss = fmap snd choices


-- Then, we check the following two properties, which is sufficient under the
-- assumption that "weakenings" does indeed enumerate *all* weakenings:
-- Lemma 2a: weakenings do indeed "naively weaken" the ifc requirement
weakeningsAreWeaker :: (Enum d, Bounded d, Ord d, Ord p) => Procedure p d -> Bool
weakeningsAreWeaker pr = and [ pr `isNaivelyStrongerThan` pr' | pr' <- weakenings pr ]

-- Lemma 2b.: if pr fullfills its ifc requirement, then also all weakenings of pr do
weakeningsAreSafe :: (Enum d, Bounded d, Ord d, Ord p) => Procedure p d -> Property
weakeningsAreSafe pr = secure hecker pr ==>
  and [ secure hecker pr' |  pr' <- weakenings pr ]




-- Now we can define a criterion which allows us to re-use existing non-interference proofs by employing *relabelings*.
-- Assume that we have
-- * an existing ifc specification (named: pr)  in terms of a set d  (of datasets)
-- * proven pr secure, by establishing all verification conditions derived by the interpretation hecker
--  (or interpretation greiner  which, by property heckerIsGreiner, is equivalent)
-- Assume moreover, that  we have
-- * a  new      ifc specification (named: pr') in terms of a set d' (of datasets)
-- * a relabeling g0, which interprets datasets ds' ∈ d' in terms of sets of datasets ds ⊆ d
--
-- We are looking for a criterion "isConsistentRelabelingFor" such that when (isConsistentRelabelingFor g0 pr pr'),
-- it is guaranteed that also pr is secure.
-- The following Definition does so by checking that after restating pr' in terms of datasets d (by appling relabeling g0),
-- the resulting ifc specification pr'Relabeled is "naively weaker" than pr.
  -- By "secureWeakeningsAreSecure" (Lemma 2), we conclude that this definition is adequate.
--
isConsistentRelabelingFor :: forall d d' p. Ord d => (d' -> Set d) -> Procedure p d -> Procedure p d' -> Bool
isConsistentRelabelingFor g0 pr pr' =  pr `isNaivelyStrongerThan` pr'Relabeled
  where g :: Set d' -> Set d
        g = gFrom g0
        pr'Relabeled :: Procedure p d
        pr'Relabeled = pr' { includes = \p -> g (includes pr' p) }




-- Unfortunately, using at least this version of "relabeling", we cannot provide for the database component
-- a generic ifc-specification in terms of datasets "Time" and "Data" from which we can infer that the ifc specification
-- in the paper is correct, since there is no consistent relabeling between those!
-- See noConsistentRelabelingGetValue from module Instances.PaperExample.ExampleOne.Noninterference

-- There may be several ways to fix this.
-- a) Take the meaning of "relabeling" as it is, but find a better Definition of isConsistentRelabelingFor.
-- b) Change the meaning of  "relabeling".
--    Maybe things work out if we take a relabeling to be a function ::
--    (d  -> Set d') instead of
--    (d' -> Set d) ?!?
--    What would the definition of isConsistentRelabelingFor be then?
-- c) abandon the concept of "relabeling"

-- In the following, i will develop option c)
-- Given two ifc specifications pr and pr', i propose to define the notion
--   pr `isStrongerThan` pr'          which, unlike
--   pr `isNaivelyStrongerThan` pr'
-- is defined even if pr is stated in terms of a set d (of datasets) *different* from d', in which pr' is stated.
-- We will then have the property (Theorem 3.):
isStrongerThanIsJustifiedUntestable ::  (Ord p, Ord d, Ord d') => Procedure p d ->  Procedure p d' -> Property
isStrongerThanIsJustifiedUntestable pr pr' =
       (
            (secure hecker pr)
        &&  (input  pr) == (input  pr')
        &&  (output pr) == (output pr')
        &&  (pr  `isStrongerThan` pr')
        &&  (∀) (input pr) (\p -> influences pr p == influences pr' p)
       )
   ==>      (secure hecker pr')

-- It is impracticable to directly check this property with QuickCheck, since the technical preconditions will almost never
-- be fullfilled. Hence we will use a generator that always produces two specifications for the "same" procedure (Theorem 3'.):
isStrongerThanIsJustified ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Property
isStrongerThanIsJustified (SpecificationPair pr pr') =
       (
            (secure hecker pr)
        &&  (pr  `isStrongerThan` pr')
       )
   ==>      (secure hecker pr')



-- Now the definition of `isStrongerThan`:
-- Given two different ifc requirements for the same procedure,
-- i.e.: given procedures pr, pr' such that
--   * input  pr == input  pr'
--   * output pr == output pr'
--   * influences pr == influences pr'
-- , the ifc requirement of pr is called "stronger" than that of pr' iff
--      pr `isStrongerThan` pr'
-- as defined here.
isStrongerThan ::  (Ord p, Ord d, Ord d') => Procedure p d ->  Procedure p d' -> Bool
pr@(Procedure { input = input, output = output, includes = includes }) `isStrongerThan` pr'@(Procedure { includes = includes' })  =
  and [ fromList [ p' | p' <- toList output, includes  p ⊆ includes  p' ]
        ⊆
        fromList [ p' | p' <- toList output, includes' p ⊆ includes' p' ]
      | p <- toList $ input]




-- The defintion can be derived from a few concept.
-- We will assume a fixed set of input and output parameters. I.e.: any
--   pr :: Procedure p d
-- we consider in this section will have the same set (input pr) of input-,
-- and the same set (output pr) of output-parameters.
-- Also, given pr :: Procedure p d, we will consider (influences pr) to be an  "implementation" of a function between these parameters.
-- Likewise, we will consider (includes pr) to be the ifc specification.

-- Concept I.:     One "implementation" of a procedure may have fewer flows than another:
hasFewerFlowsThan ::  (Ord p) => Procedure p d ->  Procedure p d' -> Bool
pr `hasFewerFlowsThan` pr'  =
      and [ influences pr p ⊆ influences pr' p | p <- toList $ input pr]


-- Concept II.:  The ifc specification part of a given procedure
-- pr :: Procedure p d
-- can be understood as an abstraction of all "implementations" that satisfy the specification.
-- Among all such "implementations", there is one that is "most-leaking" (one with the most flows),
-- i.e.: there is one implementation γ(pr) that is the least
-- upper bound (by the partial order `hasFewerFlowsThan`), given by:
γ :: (Ord d, Ord p) => Procedure p d -> Procedure p d
γ procedure@(Procedure { input, output, includes }) =
  procedure { influences = \p ->
                if (p ∈ input) then fromList [ p' | p' <- toList output, includes p ⊆ includes p' ]
                               else fromList []
            }

-- We then say that a specification pr is stronger than a specification pr' if the most-leaking implementation of pr
-- has fewer flows than the most-leaking implementation of pr':
--   pr `isStrongerThan` pr' ↔ (γ pr) `hasFewerFlowsThan` (γ pr')
-- Indeed, this is equivalent to the definition of isStrongerThan given above,
-- which is easily shown by unfolding the definition of γ.
-- Lemma 4.:
isStrongerThanIsHasFewerFlowsThan ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Bool
isStrongerThanIsHasFewerFlowsThan (SpecificationPair pr pr') =
      (γ pr) `hasFewerFlowsThan` (γ pr')
  ↔     pr  `isStrongerThan`       pr'




-- Property isStrongerThanIsJustified says that `isStrongerThan` is a "sound" criterion for re-use of specifications.
-- But is it better than `isConsistentRelabelingFor` ?!?!
-- At the minimum, it is not worse (Lemma 5.)
isStrongerThanIsBetterThanIsNaivelyStrongerThan ::  (Ord p, Ord d) => SpecificationPair p d d -> Property
isStrongerThanIsBetterThanIsNaivelyStrongerThan (SpecificationPair pr pr') =
       pr  `isNaivelyStrongerThan` pr'
  ==>  pr  `isStrongerThan`        pr'


-- To see that it is better,
-- see isStrongerThanCriterionHolds from module Instances.PaperExample.ExampleOne.Noninterference















-- It follows a section with some non-essential considerations.

-- Given an "implementation" pr, we can derive, in some sense, it's most-precise ifc specification α(pr).
-- This is "the" [1] strongest ifc specification that pr fullfills:
α :: (Ord p) => Procedure p d -> Procedure p p
α pr@(Procedure { input, output, influences}) = pr {
      includes = includes
    }
  where includes p
          | p ∈ input  = S.fromList [p]
          | p ∈ output = S.fromList [i | i <- toList input, p ∈ influences i]
          | otherwise = S.fromList [] -- TODO: require some wellformedness for procedures

-- Lemma 6. Every "implementation" pr does indeed fullfill the ifc-specification α(pr):
mostPreciseIsSecure :: (Ord p) => Procedure p d -> Bool
mostPreciseIsSecure p = secure hecker (α p)

-- Lemma 7. Every "implementation" pr is equal to the the most-leaking implementation of it's most-precise ifc-specification
γMostPreciseIsMostPrecuse :: (Ord d, Ord p) => Procedure p d -> Bool
γMostPreciseIsMostPrecuse pr = pr `eqImpl` γ (α pr)
  where pr `eqImpl` pr' = and [ influences pr p == influences pr' p | p <- toList $ input pr ]


-- Lemma 8. An auxilarry properties, that demonstrate that the definitions above are all natural:
fewerFlowsIffSecure  :: forall d p. (Ord d, Ord p) => Procedure p d -> Bool
fewerFlowsIffSecure procedure@(Procedure { input, output, includes, influences}) =
       (secure hecker procedure)
   ↔  (procedure `hasFewerFlowsThan` (γ procedure))

-- An alternative definition of γ
γ' :: (Ord d, Ord p) => Procedure p d -> Procedure p d
γ' procedure@(Procedure { input, output, includes }) =
  procedure { influences = \p ->
                if (p ∈ input) then output ∖ fromList [ p' | p' <- toList output, ds <- toList $ includes p,
                                                                                  not $ ds ∈ includes p' ]
                               else fromList []
            }
γIsγ' :: (Show d, Show p, Enum p, Bounded p, Ord d, Ord p) => Procedure p d -> Bool
γIsγ' pr = (show $ γ pr) == (show $ γ' pr) -- TODO: dont use hacky string-comparison






