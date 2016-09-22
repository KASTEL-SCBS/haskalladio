%if False
\begin{code}
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
import Data.Bool.Unicode

import Data.Set as S
import Data.Set.Unicode
import Data.Maybe (fromJust)

import qualified Data.Map as M

import Control.Monad.Writer
import Control.Monad

import Test.QuickCheck hiding (output)
\end{code}
%endif

Preliminaries
=============

\begin{code}
type OrderedSet p = (Set p, p -> p -> Bool)
\end{code}

The standard low/high lattice
\begin{code}
data LowHigh = Low
             | High
             deriving (Show, Eq, Ord, Enum, Bounded)

lowhigh :: OrderedSet LowHigh
lowhigh = (S.fromList [Low, High], (⊑))
  where Low  ⊑ Low  = True
        Low  ⊑ High = True
        High ⊑ High = True
        High ⊑ Low  = False
\end{code}


Verification of IFC Specifications
==================================

An verification condition (derived from a ifc specification) is
a "classical" non-interference condition, and hence consists of

* a lattice structure on on the set `l` of security levels
* a mapping from parameters to their security level in `l`

\begin{code}
type VerificationCondition p l = (OrderedSet l, p -> l)
\end{code}

A SpecificationInterpretation gives a ifc specification meaning by
deriving a number of verification conditions that all have to hold
for the ifc specification to hold.
\begin{code}
type SpecificationInterpretation p d l = Procedure p d -> [VerificationCondition p l]
\end{code}

The approach for KeY is to translate the ifc-specification into several non-interference verification
condition, each for the `LowHigh` lattice.
\begin{code}
key :: (Ord d, Ord p) => SpecificationInterpretation p d LowHigh
key pr@(Procedure { input, output, includes, influences}) =
    [ (lowhigh, (\p -> if (d ∈ includes p) then High else Low)) | d <- toList $ datasets pr]
\end{code}

The approach for JOANA generates just one JOANA-Specification, using the powerset-lattice of `d`.
\begin{code}
joana :: (Ord d, Ord p) => SpecificationInterpretation p d (Set d)
joana pr@(Procedure { input, output, includes, influences}) = [((powerset (datasets pr), (⊆)), includes )]
\end{code}

In this simplified model, a procedures implementation is abstractly defined by its  information-flow
between input and output variables, as defined by the function `influences`.
Hence, a verification condition holds if this flow does not exceed that allowed by the flow lattice:
\begin{code}
holds :: (Ord p) => Procedure p d -> VerificationCondition p l -> Bool
holds (Procedure { input, output, includes, influences}) ((l,(⊑)), classifiedAs) =
    (∀) input (\i ->  (∀) output (\o ->
          (o ∈ (influences i)) → (classifiedAs i ⊑ classifiedAs o)
    ))
\end{code}

Given an ifc-specification interpretation (e.g. `joana` or `key`),
a procedure pr is secure iff all its verification conditions hold.
\begin{code}
secure :: (Ord p) => SpecificationInterpretation p d l -> Procedure p d -> Bool
secure interpretation pr = (∀) (interpretation pr) (\condition -> holds pr condition)
\end{code}

`joana` and `key` are equivalent! Otherwise, we couldn't use KeY and JOANA interchangably!!
\begin{code}
joanaIsKey :: (Ord d, Ord p) => Procedure p d -> Bool
joanaIsKey pr = secure joana pr ⇔ secure key pr
\end{code}


Specifically, a procedure being secure is characerized as:
\begin{code}
secureCharactization :: forall d p. (Ord d, Ord p) => Procedure p d -> Bool
secureCharactization pr@(Procedure { input, output, includes, influences }) =
       (secure key pr)
   ⇔  (∀) input (\i ->  (∀) output (\o ->
          (o ∈ influences i) → (includes i ⊆ includes o)
       ))
\end{code}



Criteria for Re-use of existing Non-inteference Proofs
=======================================================

Before we can we define a criterion which allows us to re-use existing non-interference proofs by employing *relabelings*,
we introduce the naive concept of "weak / strong" ifc-specifications:

"weak / strong" IFC-Specifications  (naively)
--------------------------------------------

Given two different ifc specifications for the same procedure using the *same* datasets `d`,
i.e.: given procedures `pr`, `pr'` of type `Procedure p d` such that

  * `input  pr == input  pr'`
  * `output pr == output pr'`
  * `influences pr == influences pr'`

the ifc specification of `pr` is called "naively stronger" than that of `pr'` iff

    pr isNaivelyStrongerThan pr'

as defined here:
\begin{code}
isNaivelyStrongerThan ::  (Ord d) => Procedure p d ->  Procedure p d -> Bool
pr `isNaivelyStrongerThan` pr'  =
      (∀) (input pr)  (\i -> includes pr' i ⊆ includes pr i)
  ∧   (∀) (output pr) (\o -> includes pr' o ⊇ includes pr o)
\end{code}

Weakenings of secure ifc-specifications are secure:
\begin{code}
secureWeakeningsAreSecure :: (Enum d, Bounded d, Ord d, Ord p) => Procedure p d -> Procedure p d -> Property
secureWeakeningsAreSecure pr pr' =
  (secure joana pr ) ∧ (pr `isNaivelyStrongerThan` pr')
  ==>
  (secure joana pr')
\end{code}


%if False
unfortunately, naively checking this using QuickCheck is inefficient,
even if we test the following:
\begin{code}
secureWeakeningsAreSecure' :: (Enum d, Bounded d, Ord d, Ord p) => SpecificationPair p d d -> Property
secureWeakeningsAreSecure' (SpecificationPair pr pr') =
  (secure joana pr ) ∧ (pr `isNaivelyStrongerThan` pr')
  ==>
  (secure joana pr')
\end{code}


Instead, we define the enumeration of all "weakenings" of a given ifc specification:
"weakings pr" enumerates all weakenings of pr, i.e. all procedures pr' such that
     pr `isNaivelyStrongerThan` pr'
\begin{code}
weakenings :: (Ord d, Ord p, Enum d, Bounded d) => Procedure p d -> [Procedure p d]
\end{code}
\begin{code}
weakenings pr@(Procedure { input, output, includes, influences}) =
   [ pr { includes = \p -> fromJust $ lookup p choice } | choice <- choices ]
  where choices = chooseOneEach $    [(i, [d | d <- toList $ powerset $ fromList allValues, d ⊆ includes i]) | i <- toList $  input]
                                  ++ [(o, [d | d <- toList $ powerset $ fromList allValues, d ⊇ includes o]) | o <- toList $ output]

        chooseOneEach :: [(a,[b])] -> [[(a,b)]]
        chooseOneEach choices = fmap (zip as) $ sequence bss
          where as  = fmap fst choices
                bss = fmap snd choices
\end{code}

Then, we check the following three properties:
\begin{code}
weakeningsAreWeaker :: (Enum d, Bounded d, Ord d, Ord p) => Procedure p d -> Bool
weakeningsAreWeaker pr = (∀) (weakenings pr) (\pr' -> pr `isNaivelyStrongerThan` pr')
\end{code}

\begin{code}
weakerAreWeakenings :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => SpecificationPair p d d -> Property
weakerAreWeakenings (SpecificationPair pr pr') =
     pr `isNaivelyStrongerThan` pr'
 ==> (∃) (weakenings pr) (\prw ->  (show $ prw) == (show $ pr')) -- TODO: dont use hacky string-comparison
\end{code}


If pr fullfills its ifc requirement, then also all weakenings of pr do
\begin{code}
weakeningsAreSafe :: (Enum d, Bounded d, Ord d, Ord p) => Procedure p d -> Property
weakeningsAreSafe pr = secure joana pr ==>
  (∀) (weakenings pr) (\pr' -> secure joana pr')
\end{code}
%endif


Criteria using Relabelings
---------------------------

Now we try define a criterion which allows us to re-use existing non-interference proofs by employing *relabelings*.
Assume that we have

* an existing ifc specification `pr`  in terms of a set `d`  (of datasets)
* proven `pr` secure, by establishing all verification conditions derived by the interpretation `joana`
 (or interpretation `greiner`  which, by property `joanaIsGreiner`, is equivalent)

Assume moreover, that  we have

  * a  new    ifc specification `pr'` in terms of a set `d'` (of datasets)
  * a  "relabeling"

We are looking for a criterion `isConsistentRelabelingFor` such that when  `isConsistentRelabelingFor r pr pr'`
for some relabeling `r`, then it is guaranteed that also `pr` is secure.


Relabelings - a wrong Definition
---------------------------------

In a first attempt, let us define a relabeling to be a function `g0` which interprets datasets

     ds' ∈ d' in terms of sets of datasets ds ⊆ d

Of course, simpler relabelings that interprets datasets

     ds' ∈ d' in terms of         datasets ds ∈ d

are a special case of this notion of "relabeling".

The following definition of `isConsistentRelabelingFor` checks that after restating `pr'` in terms of datasets `d` (by appling relabeling `g0`),
the resulting ifc specification is "naively weaker" than `pr`.

\begin{code}
isConsistentRelabelingFor :: forall d d' p. Ord d => (d' -> Set d) -> Procedure p d -> Procedure p d' -> Bool
isConsistentRelabelingFor g0 pr pr' =  pr `isNaivelyStrongerThan` (pr' `relabeledUsing` g0)
\end{code}

with the application of a relabeling defined as:
\begin{code}
relabeledUsing :: forall p d d'. (Ord d) =>  Procedure p d' -> (d' -> Set d) -> Procedure p d
pr' `relabeledUsing` g0 = pr' { includes = \p -> g (includes pr' p) }
  where g :: Set d' -> Set d
        g ds' = (⋃) [ g0 d' | d' <- toList ds']

\end{code}


Unfortunately, this criterion is neither sound nor complete, i.e.:

Neither does the following property hold ...
\begin{code}
existsConsistentRelabelingIsJustified ::  (Ord p, Ord d, Ord d') => Procedure p d -> Procedure p d' -> Property
existsConsistentRelabelingIsJustified pr pr' =
       (
          (input  pr) == (input  pr')
        ∧ (output pr) == (output pr')
        ∧ (∀) (input pr) (\p -> influences pr p == influences pr' p)

        ∧ (secure joana pr)
        ∧ (∃) relabelings (\g0 -> isConsistentRelabelingFor g0 pr pr')
       )
   ==>    (secure joana pr')

  where relabelings = setFunctionsBetween (datasets pr) (datasets pr')
\end{code}

%if False
It is impracticable to directly check this property with QuickCheck, since the technical preconditions will almost never
be fullfilled. Hence we will use a generator that always produces two specifications for the "same" procedure.
\begin{code}
existsConsistentRelabelingIsJustifiedTestable ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Property
existsConsistentRelabelingIsJustifiedTestable (SpecificationPair pr pr') =
       (
          (secure joana pr)
        ∧ (∃) relabelings (\g0 -> isConsistentRelabelingFor g0 pr pr')
       )
   ==>    (secure joana pr')

  where relabelings = setFunctionsBetween (datasets pr) (datasets pr')
\end{code}
%endif

.. nor does this:
\begin{code}
existsConsistentRelabelingIsComplete ::  (Ord p, Ord d, Ord d') => Procedure p d -> Procedure p d' -> Property
existsConsistentRelabelingIsComplete pr pr' =
       (
          (input  pr) == (input  pr')
        ∧ (output pr) == (output pr')
        ∧ (∀) (input pr) (\p -> influences pr p == influences pr' p)

        ∧ (secure joana pr)
        ∧ (secure joana pr')
       )
   ==>    (∃) relabelings (\g0 -> isConsistentRelabelingFor g0 pr pr')

  where relabelings = setFunctionsBetween (datasets pr) (datasets pr')
\end{code}

%if False
\begin{code}
existsConsistentRelabelingIsCompleteTestable ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Property
existsConsistentRelabelingIsCompleteTestable (SpecificationPair pr pr') =
       (
          (secure joana pr)
        ∧ (secure joana pr')
       )
   ==>    (∃) relabelings (\g0 -> isConsistentRelabelingFor g0 pr pr')

  where relabelings = setFunctionsBetween (datasets pr) (datasets pr')
\end{code}
%endif

Specifically, we cannot provide for the database component from the paper example
a generic ifc-specification in terms of datasets "Time" and "Data" from which we can infer that the ifc specification
in the paper is correct, since there is no consistent relabeling between those!

See `noConsistentRelabelingGetValue` from module `Instances.PaperExample.ExampleOne.Noninterference`




There may be several ways to fix this.

 1. Take the meaning of "relabeling" as it is, but find a better Definition of `isConsistentRelabelingFor`.
 2. Change the meaning of  "relabeling"
 3. Abandon the concept of "relabeling"


First, i will develop Option 2, then Option 3.
I do not know a way to fix things via Option 1.


A Criterion using Relabelings
-----------------------------
We now "reverse" the dircetion of relabelings, i.e., we define a relabeling to be a function `f0` which interprets datasets

     ds ∈ d in terms of sets of datasets ds' ⊆ d'

Of course, simpler relabelings that interprets datasets

     ds ∈ d in terms of         datasets ds' ∈ d'

are again a special case of this notion of "relabeling".

The analogous Definition of `isConsistentRelabelingFor` reads:

\begin{code}
isConsistentRelabelingRevFor :: forall d d' p. Ord d' => (d -> Set d') -> Procedure p d -> Procedure p d' -> Bool
isConsistentRelabelingRevFor f0 pr pr' =  (pr `relabeledRevUsing` f0) `isNaivelyStrongerThan` pr'
\end{code}

with the application of a relabeling defined as:
\begin{code}
relabeledRevUsing :: forall p d d'. (Ord d') =>  Procedure p d -> (d -> Set d') -> Procedure p d'
pr `relabeledRevUsing` f0 =  pr { includes = \p -> f (includes pr p) }
  where f :: Set d -> Set d'
        f ds = (⋃) [ f0 d | d <- toList ds]
\end{code}


This criterion *is* Sound:
\begin{code}
existsConsistentRelabelingRevIsJustified ::  (Ord p, Ord d, Ord d') => Procedure p d -> Procedure p d' -> Property
existsConsistentRelabelingRevIsJustified pr pr' =
       (
          (input  pr) == (input  pr')
        ∧ (output pr) == (output pr')
        ∧ (∀) (input pr) (\p -> influences pr p == influences pr' p)

        ∧ (secure joana pr)
        ∧ (∃) relabelings (\f0 -> isConsistentRelabelingRevFor f0 pr pr')
       )
   ==>    (secure joana pr')

  where relabelings = setFunctionsBetween (datasets pr') (datasets pr)
\end{code}

%if False
\begin{code}
existsConsistentRelabelingRevIsJustifiedTestable ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Property
existsConsistentRelabelingRevIsJustifiedTestable (SpecificationPair pr pr') =
       (
          (secure joana pr)
        ∧ (∃) relabelings (\f0 -> isConsistentRelabelingRevFor f0 pr pr')
       )
   ==>    (secure joana pr')

  where relabelings = setFunctionsBetween (datasets pr') (datasets pr)
\end{code}
%endif

.. but not complete, i.e., the following property does *not* hold:
\begin{code}
existsConsistentRelabelingRevIsComplete ::  (Ord p, Ord d, Ord d') => Procedure p d -> Procedure p d' -> Property
existsConsistentRelabelingRevIsComplete pr pr' =
       (
          (input  pr) == (input  pr')
        ∧ (output pr) == (output pr')
        ∧ (∀) (input pr) (\p -> influences pr p == influences pr' p)

        ∧ (secure joana pr)
        ∧ (secure joana pr')
       )
   ==>    (∃) relabelings (\f0 -> isConsistentRelabelingRevFor f0 pr pr')

  where relabelings = setFunctionsBetween (datasets pr') (datasets pr)
\end{code}

%if False
\begin{code}
existsConsistentRelabelingRevIsCompleteTestable ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Property
existsConsistentRelabelingRevIsCompleteTestable (SpecificationPair pr pr') =
       (
          (secure joana pr)
        ∧ (secure joana pr')
       )
   ==>    (∃) relabelings (\f0 -> isConsistentRelabelingRevFor f0 pr pr')

  where relabelings = setFunctionsBetween (datasets pr') (datasets pr)
\end{code}
%endif



Note that i'm currently don't know how to directly justify the soundness of this criterion.
An indirect justifcation stems from the following property (see below for the definition of `isStrongerThan`).
\begin{code}
relabeleingsRevAreStrongerThan ::  (Ord p, Ord d, Ord d') => Set d' -> Procedure p d -> Bool
relabeleingsRevAreStrongerThan ds' pr =
   (∀) relabelings (\f0 -> pr `isStrongerThan` (pr `relabeledRevUsing` f0))
  where relabelings = setFunctionsBetween ds' (datasets pr)
\end{code}

%if False
\begin{code}
setFunctionsBetween :: (Ord d, Ord d') => (Set d) -> (Set d') -> [ d' -> Set d]
setFunctionsBetween ds ds' =
    fmap ((M.!) . (M.fromList)) $
    chooseOneEach $ [(d', toList $ powerset ds) | d' <- toList ds']
  where chooseOneEach :: [(a,[b])] -> [[(a,b)]]
        chooseOneEach choices = fmap (zip as) $ sequence bss
          where as  = fmap fst choices
                bss = fmap snd choices
\end{code}

\begin{code}
existsConsistentRelabelingFor :: forall d d' p. (Ord d, Ord d', Ord p) => Procedure p d -> Procedure p d' -> Bool
existsConsistentRelabelingFor pr pr' =  (∃) relabelings (\g0 -> isConsistentRelabelingFor g0 pr pr')
  where relabelings = setFunctionsBetween (datasets pr) (datasets pr')

existsConsistentRelabelingRevFor :: forall d d' p. (Ord d, Ord d', Ord p) => Procedure p d -> Procedure p d' -> Bool
existsConsistentRelabelingRevFor pr pr' =  (∃) relabelings (\g0 -> isConsistentRelabelingRevFor g0 pr pr')
  where relabelings = setFunctionsBetween (datasets pr') (datasets pr)
\end{code}
%endif


A Criterion without relabelings
-------------------------------
I will now develope Option 3.

Note that the sound (but not complete) relabeling-based criterion ("Option 2.") suggest two method of proof-reuse, given `pr` and `pr'`:

 1. let the user specifiy a relabeling candidate f0, and check whether

        isConsistentRelabelingRevFor f0 pr pr'

    holds.
 2. Insteam of letting the user specifiy the candiate, enumerate all possible relabeling candidates `f0` and check them.
    If `pr` specified in terms of `n` datasets, and `pr'` in terms of `m` datasets, there are `2^(n+m)` possible candidates.

Is there a sound and criterion that avoids the need of relabelings, but is at least as complete?

Given two ifc specifications `pr` and `pr'`, i propose to define the notion

        pr isStrongerThan        pr'           which, unlike
        pr isNaivelyStrongerThan pr'

is defined even if `pr` is stated in terms of a set `d` (of datasets) *different* from `d'` (the set of dataterms in which `pr'` is stated).

We will then have the soundness property:
\begin{code}
isStrongerThanIsJustified ::  (Ord p, Ord d, Ord d') => Procedure p d ->  Procedure p d' -> Property
isStrongerThanIsJustified pr pr' =
       (
          (secure joana pr)
        ∧ (input  pr) == (input  pr')
        ∧ (output pr) == (output pr')

        ∧ (∀) (input pr) (\p -> influences pr p == influences pr' p)
        ∧ (pr  `isStrongerThan` pr')
       )
   ==>      (secure joana pr')
\end{code}

%if False
It is impracticable to directly check this property with QuickCheck, since the technical preconditions will almost never
be fullfilled. Hence we will use a generator that always produces two specifications for the "same" procedure:
\begin{code}
isStrongerThanIsJustifiedTestable ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Property
isStrongerThanIsJustifiedTestable (SpecificationPair pr pr') =
       (
            (secure joana pr)
        &&  (pr  `isStrongerThan` pr')
       )
   ==>      (secure joana pr')
\end{code}
%endif

.. but *not* the completeness Property:
\begin{code}
isStrongerThanIsComplete ::  (Ord p, Ord d, Ord d') => Procedure p d ->  Procedure p d' -> Property
isStrongerThanIsComplete pr pr' =
       (
          (secure joana pr)
        ∧ (input  pr) == (input  pr')
        ∧ (output pr) == (output pr')

        ∧ (∀) (input pr) (\p -> influences pr p == influences pr' p)
        ∧ (secure joana pr')
       )
   ==>    (pr  `isStrongerThan` pr')
\end{code}

%if False
\begin{code}
isStrongerThanIsCompleteTestable ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Property
isStrongerThanIsCompleteTestable (SpecificationPair pr pr') =
       (
          (secure joana pr)
        ∧ (secure joana pr')
       )
   ==>    (pr  `isStrongerThan` pr')
\end{code}
%endif

In fact, Option 3.  will tourn out to be strictly "better" than Option 2.:

It will hold that:
\begin{code}
isStrongerThanBetterThanConsistentRelabelingRevFor ::  (Ord p, Ord d, Ord d') => Procedure p d ->  Procedure p d' ->  Bool
isStrongerThanBetterThanConsistentRelabelingRevFor pr pr' =
       (
          (input  pr) == (input  pr')
        ∧ (output pr) == (output pr')
        ∧ (∀) (input pr) (\p -> influences pr p == influences pr' p)

        ∧ (∃) relabelings (\f0 -> isConsistentRelabelingRevFor f0 pr pr')
       )
    →    (pr  `isStrongerThan` pr')
  where relabelings = setFunctionsBetween (datasets pr') (datasets pr)
\end{code}

%if False
\begin{code}
isStrongerThanBetterThanConsistentRelabelingRevForTestable ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Bool
isStrongerThanBetterThanConsistentRelabelingRevForTestable (SpecificationPair pr pr') =
          (∃) relabelings (\f0 -> isConsistentRelabelingRevFor f0 pr pr')
    →    (pr  `isStrongerThan` pr')
  where relabelings = setFunctionsBetween (datasets pr') (datasets pr)
\end{code}
%endif



... but *not* that:
\begin{code}
consistentRelabelingRevForBetterThanIsStrongerThan ::  (Ord p, Ord d, Ord d') =>  Procedure p d ->  Procedure p d' ->  Bool
consistentRelabelingRevForBetterThanIsStrongerThan pr pr' =
       (
          (input  pr) == (input  pr')
        ∧ (output pr) == (output pr')
        ∧ (∀) (input pr) (\p -> influences pr p == influences pr' p)

        ∧ (pr  `isStrongerThan` pr')
       )
    →    (∃) relabelings (\f0 -> isConsistentRelabelingRevFor f0 pr pr')
  where relabelings = setFunctionsBetween (datasets pr') (datasets pr)
\end{code}

%if False
\begin{code}
consistentRelabelingRevForBetterThanIsStrongerThanTestable ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Bool
consistentRelabelingRevForBetterThanIsStrongerThanTestable (SpecificationPair pr pr') =
          (pr  `isStrongerThan` pr')
    →    (∃) relabelings (\f0 -> isConsistentRelabelingRevFor f0 pr pr')
  where relabelings = setFunctionsBetween (datasets pr') (datasets pr)
\end{code}
%endif



Now the definition of `isStrongerThan`:

Given two different ifc specifications for the same procedure using possibly different datasets `d` and `d'`, i.e.:
given procedure `pr` of type `Procedure p d` and procedure `pr'` of type `Procedure p d'`  such that

  * `input  pr == input  pr'`
  * `output pr == output pr'`
  * `influences pr == influences pr'`

the ifc specification of `pr` is called "stronger" than that of `pr'` iff
for all input parameters `i`, the set of output parameters `o` that

  * by specification `pr` include at least those datasets included in `i`

is included in the set of of output parameters `o` that

  * by specification *`pr'`* include at least those datasets included in `i`

\begin{code}
isStrongerThan ::  (Ord p, Ord d, Ord d') => Procedure p d ->  Procedure p d' -> Bool
pr@(Procedure { input = input, output = output, includes = includes }) `isStrongerThan` pr'@(Procedure { includes = includes' })  =
  (∀) input  (\i ->
        fromList [ o | o <- toList output, includes  i ⊆ includes  o ]
        ⊆
        fromList [ o | o <- toList output, includes' i ⊆ includes' o ]
  )
\end{code}

Deriviation of this Criterion
-----------------------------

This Criterion can be derived from a few concept. Doing this,
we will assume a fixed set of input and output parameters. I.e.: any

    pr :: Procedure p d

we consider in this section will have the same set `(input pr)` of input-,
and the same set `(output pr)` of output-parameters.

Also, given `pr :: Procedure p d`, we will consider `(influences pr)` to be an "implementation" of a function between these parameters.
Likewise, we will consider `(includes pr)` to be the ifc specification.

Concept I.:     One "implementation" of a procedure may have fewer flows than another:
\begin{code}
hasFewerFlowsThan ::  (Ord p) => Procedure p d ->  Procedure p d' -> Bool
pr `hasFewerFlowsThan` pr'  =
      (∀) (input pr) (\i -> influences pr i ⊆ influences pr' i )
\end{code}



Concept II.:  The ifc specification part of a given procedure

    pr :: Procedure p d

can be understood as an abstraction of all "implementations" that satisfy the specification.
Among all such "implementations", there is one that is "most-leaking" (one with the most flows),
i.e.: there is one implementation `γ(pr)` that is the least
upper bound (by the partial order `hasFewerFlowsThan`) of all such implementations.

It is given by:
\begin{code}
γ :: (Ord d, Ord p) => Procedure p d -> Procedure p d
γ procedure@(Procedure { input, output, includes }) =
  procedure { influences = \p ->
                if (p ∈ input) then fromList [ p' | p' <- toList output, includes p ⊆ includes p' ]
                               else fromList []
            }
\end{code}

We then say that a specification `pr` is stronger than a specification `pr'` if the most-leaking implementation of `pr`
has fewer flows than the most-leaking implementation of `pr'`:

    pr `isStrongerThan` pr' ⇔ (γ pr) `hasFewerFlowsThan` (γ pr')

Indeed, this is equivalent to the definition of `isStrongerThan` given above,
which is easily shown by unfolding the definition of `γ`.

\begin{code}
isStrongerThanIsHasFewerFlowsThan ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Bool
isStrongerThanIsHasFewerFlowsThan (SpecificationPair pr pr') =
      (γ pr) `hasFewerFlowsThan` (γ pr')
  ⇔     pr  `isStrongerThan`       pr'
\end{code}



Property `isStrongerThanIsJustified` says that `isStrongerThan` is a "sound" criterion for re-use of specifications.
But is it better than its relabeling counter-part `isConsistentRelabelingFor` ?!?!
At the minimum, it is not worse, in the following sense:
\begin{code}
isStrongerThanIsBetterThanIsNaivelyStrongerThan ::  (Ord p, Ord d) => SpecificationPair p d d -> Property
isStrongerThanIsBetterThanIsNaivelyStrongerThan (SpecificationPair pr pr') =
       pr  `isNaivelyStrongerThan` pr'
  ==>  pr  `isStrongerThan`        pr'
\end{code}

To see that it is better note that in the paper example, the generic ifc-specification in terms of datasets "Time" and "Data" *is*
stronger than the ifc-specification in terms of "Consumptiondata", and hence we can re-use it!
See `isStrongerThanCriterionHoldsGetValue` from module `Instances.PaperExample.ExampleOne.Noninterference`.


Appendix
========

It follows a section with some non-essential considerations.

Given an "implementation" `pr`, we can derive, in some sense, it's most-precise ifc specification `α(pr)`.
This is in some sense "the" strongest ifc specification that `pr` fullfills, and it's derived by simply labeling
all output parameter by the set of input parameters that influence it.
\begin{code}
α :: (Ord p) => Procedure p d -> Procedure p p
α pr@(Procedure { input, output, influences}) = pr {
      includes = includes
    }
  where includes p
          | p ∈ input  = fromList [p]
          | p ∈ output = fromList [i | i <- toList input, p ∈ influences i]
          | otherwise  = fromList [] -- TODO: require some wellformedness for procedures
\end{code}


Every "implementation" `pr` does indeed fullfill the ifc-specification `α(pr)`:
\begin{code}
mostPreciseIsSecure :: (Ord p) => Procedure p d -> Bool
mostPreciseIsSecure p = secure joana (α p)
\end{code}

Every "implementation" `pr` is equal to the the most-leaking implementation of it's most-precise ifc-specification
\begin{code}
γMostPreciseIsMostPrecuse :: (Ord d, Ord p) => Procedure p d -> Bool
γMostPreciseIsMostPrecuse pr = pr `eqImpl` γ (α pr)
  where pr `eqImpl` pr' = (∀) (input pr) (\p -> influences pr p == influences pr' p)
\end{code}

An auxilarry properties that demonstrate that the definitions above are all natural:

A procedure  `pr` is secure iff it has fewer Flows than the most-leaking implementation of the ifc-specification of `pr`.
\begin{code}
fewerFlowsIffSecure  :: forall d p. (Ord d, Ord p) => Procedure p d -> Bool
fewerFlowsIffSecure pr =
      (secure joana pr)
   ⇔  (pr `hasFewerFlowsThan` (γ pr))
\end{code}


An alternative definition of γ
\begin{code}
γ' :: (Ord d, Ord p) => Procedure p d -> Procedure p d
γ' procedure@(Procedure { input, output, includes }) =
  procedure { influences = \p ->
                if (p ∈ input) then output ∖ fromList [ p' | p' <- toList output, ds <- toList $ includes p,
                                                                                  not $ ds ∈ includes p' ]
                               else fromList []
            }

γIsγ' :: (Show d, Show p, Enum p, Bounded p, Ord d, Ord p) => Procedure p d -> Bool
γIsγ' pr = (show $ γ pr) == (show $ γ' pr) -- TODO: dont use hacky string-comparison
\end{code}



Strongest Guarantee w.r.t. weakened Assumptions
-----------------------------------------------

A given ifc-specification is comprised of "Assumptions" and "Guarantees":

 * `includes` restricted to `input`   parameters specifies assumptions on the information content of `input` paramaters
 * `includes` restricted to `output`  parameters specifies guarantees made by the program on the the information content of `output` paramaters


A natural quesion to ask is what guarantees still hold given a "weakening of assumptions".

Based on the notiong of `isNaivelyStrongerThan`, we define

    `pr makesWeakerAssumptionsThan pr'`

and

    `pr makesStrongerGuaranteesThan pr'`

to mean:
\begin{code}
makesWeakerAssumptionsThan ::  (Ord d) => Procedure p d ->  Procedure p d -> Bool
pr `makesWeakerAssumptionsThan` pr'  =
      (∀) (input pr)  (\i -> includes pr' i ⊆ includes pr i)

makesStrongerGuaranteesThan ::  (Ord d) => Procedure p d ->  Procedure p d -> Bool
pr `makesStrongerGuaranteesThan` pr'  =
      (∀) (output pr) (\o -> includes pr' o ⊇ includes pr o)
\end{code}

i.e. we have:
\begin{code}
weakerStongerIsNaively :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Procedure p d -> Procedure p d -> Bool
weakerStongerIsNaively  pr pr' =
      pr `isNaivelyStrongerThan` pr'
  ⇔ (pr `makesWeakerAssumptionsThan`  pr'
    ∧ pr `makesStrongerGuaranteesThan` pr' )
\end{code}


Then, the strongest guarantee still valid given a weakening `pr'` of assumptions wrt. original specification `pr` is obtained by considereing the most-leaking-implementation of  the original specification `pr`

\begin{code}
strongestValidGuarantee :: (Ord d, Ord p) => Procedure p d -> Procedure p d -> Procedure p d
strongestValidGuarantee pr@(Procedure { input, output }) pr' = pr {
    includes = \p -> if (p ∈ input) then
                       (includes pr' p)
                     else
                       (⋃) [ includes pr' i  | i <- toList input, p ∈ (influences mostLeaking i)]
    }
  where mostLeaking = γ pr
\end{code}

This is indeed valid:

\begin{code}
strongestValidGuaranteeIsValid :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Procedure p d -> Procedure p d -> Property
strongestValidGuaranteeIsValid pr pr' =
      (secure joana pr
    ∧  pr' `makesWeakerAssumptionsThan` pr )
  ==>
      (secure joana $ strongestValidGuarantee pr pr')
\end{code}

Note that it is *not "extensive" in the following sense:

\begin{code}
strongestValidGuaranteeIsExtensive :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Procedure p d -> Procedure p d -> Property
strongestValidGuaranteeIsExtensive pr pr' =
      (secure joana pr
    ∧  pr' `makesWeakerAssumptionsThan` pr )
  ==>
       pr `makesStrongerGuaranteesThan` (strongestValidGuarantee pr pr')
\end{code}

since `pr` may include datasets in one of it's output parameters that are included in none of its input parameters.

Once can, of course, define an "extensive" operator like this:


\begin{code}
strongestValidGuaranteeExtensive :: (Ord d, Ord p) => Procedure p d -> Procedure p d -> Procedure p d
strongestValidGuaranteeExtensive pr@(Procedure { input, output }) pr' = pr'' {
    includes = \p -> if (p ∈ input) then
                       (includes pr'' p)
                     else
                       (includes pr p) ∪ (includes pr'' p)
    }
  where pr'' = strongestValidGuarantee pr pr'
\end{code}


such that both:

\begin{code}
strongestValidGuaranteeExtensiveIsExtensive :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Procedure p d -> Procedure p d -> Property
strongestValidGuaranteeExtensiveIsExtensive pr pr' =
       pr' `makesWeakerAssumptionsThan` pr
  ==>
       pr  `makesStrongerGuaranteesThan` (strongestValidGuaranteeExtensive pr pr')
\end{code}


and:

\begin{code}
strongestValidGuaranteeExtensiveIsValid :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Procedure p d -> Procedure p d -> Property
strongestValidGuaranteeExtensiveIsValid pr pr' =
      (secure joana pr
    ∧  pr' `makesWeakerAssumptionsThan` pr )
  ==>
      (secure joana $ strongestValidGuaranteeExtensive pr pr')
\end{code}


Both operator are idempotent:

\begin{code}
strongestValidGuaranteeExtensiveIsIdempotent :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Procedure p d -> Procedure p d -> Property
strongestValidGuaranteeExtensiveIsIdempotent pr pr' =
       pr' `makesWeakerAssumptionsThan` pr
  ==>
      (show $ strongestValidGuaranteeExtensive pr                                         pr') ==
      (show $ strongestValidGuaranteeExtensive (strongestValidGuaranteeExtensive pr pr')  pr')      -- TODO: dont use hacky string-comparison
\end{code}

\begin{code}
strongestValidGuaranteeIsIdempotent :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Procedure p d -> Procedure p d -> Property
strongestValidGuaranteeIsIdempotent pr pr' =
       pr' `makesWeakerAssumptionsThan` pr
  ==>
      (show $ strongestValidGuarantee pr                                pr') ==
      (show $ strongestValidGuarantee (strongestValidGuarantee pr pr')  pr')      -- TODO: dont use hacky string-comparison
\end{code}

