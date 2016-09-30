%if False
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module NoninterferenceGreiner where

import Noninterference.Util
import Noninterference.Procedure
import Noninterference.Testgen

import Prelude as P

import Algebra.Lattice
import Unicode
import Data.Bool.Unicode

import Data.List (sortBy)
import Data.Ord (comparing)
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

A SpecificationInterpretation gives meaning to an  ifc specification by
deriving a number of verification conditions that all have to hold
for the ifc specification to hold.
\begin{code}
type SpecificationInterpretation p d l = Specification p d -> [VerificationCondition p l]
\end{code}

The approach for KeY is to translate the ifc-specification into several non-interference verification
condition, each for the `LowHigh` lattice.
\begin{code}
key :: (Ord d, Ord p) => SpecificationInterpretation p d LowHigh
key pr@(Specification { includes, datasets }) =
    [ (lowhigh, (\p -> if (d ∈ includes p) then Low else High)) | d <- toList $ datasets ]
\end{code}

The approach for JOANA generates just one JOANA-Specification, using the reversed powerset-lattice of `d`.
\begin{code}
joana :: (Ord d, Ord p) => SpecificationInterpretation p d (Set d)
joana pr@(Specification { includes, datasets }) = [((powerset datasets, (⊇)), includes )]
\end{code}

In this simplified model, a procedures implementation is abstractly defined by its information-flow
between input and output variables, as defined by the function `influences`.
Hence, a verification condition holds if this flow does not exceed that allowed by the flow lattice:
\begin{code}
holds :: (Ord p) => Component p -> Implementation p -> VerificationCondition p l -> Bool
holds (Component { input, output }) (Implementation { influences }) ((l,(⊑)), classifiedAs) =
    (∀) input (\i ->  (∀) output (\o ->
          (o ∈ (influences i)) → (classifiedAs i ⊑ classifiedAs o)
    ))
\end{code}

Given an ifc-specification interpretation (e.g. `joana` or `key`),
a procedure pr is secure iff all its verification conditions hold.
\begin{code}
secure :: (Ord p) => SpecificationInterpretation p d l -> Component p -> Implementation p -> Specification p d -> Bool
secure interpretation pr impl sp = (∀) (interpretation sp) (\condition -> holds pr impl condition)
\end{code}

`joana` and `key` are equivalent! Otherwise, we couldn't use KeY and JOANA interchangably!!
\begin{code}
joanaIsKey :: (Ord d, Ord p) => Component p -> Implementation p -> Specification p d -> Bool
joanaIsKey pr impl sp = secure joana pr impl sp ⇔ secure key pr impl sp
\end{code}


Specifically, a procedure being secure is characerized as:
\begin{code}
secureCharactization :: forall d p. (Ord d, Ord p) => Component p -> Implementation p -> Specification p d -> Bool
secureCharactization pr@(Component { input, output }) impl@(Implementation { influences }) sp@(Specification { includes }) =
       (secure key pr impl sp)
   ⇔  (∀) input (\i ->  (∀) output (\o ->
          (o ∈ influences i) → (includes i ⊇ includes o)
       ))
\end{code}



Criteria for Re-use of existing Non-inteference Proofs
=======================================================

Before we can we define a criterion which allows us to re-use existing non-interference proofs by employing *relabelings*,
we introduce the naive concept of "weak / strong" ifc-specifications:

"weak / strong" IFC-Specifications  (naively)
--------------------------------------------

Given two different ifc specifications for the same procedure `pr` using the *same* datasets `d`,
i.e.: given specifications `sp`, `sp'` of type `Specification p d`,
the ifc specification `sp` (for `pr`) is called "naively stronger" than the specification `sp'` (for `pr`) iff

    sp `(isNaivelyStrongerThanFor pr)` sp'

as defined here:
\begin{code}
isNaivelyStrongerThanFor ::  (Ord d) => Component p -> Specification p d -> Specification p d -> Bool
isNaivelyStrongerThanFor pr sp sp' =
      (∀) (input pr)  (\i -> includes sp' i ⊇ includes sp i)
  ∧   (∀) (output pr) (\o -> includes sp' o ⊆ includes sp o)
\end{code}

Secure implementations  remain secure if a specification is weakened:
\begin{code}
secureWeakeningsAreSecure :: (Enum d, Bounded d, Ord d, Ord p) => Component p -> Implementation p -> Specification p d -> Specification p d -> Property
secureWeakeningsAreSecure pr impl sp sp' =
  (secure joana pr impl sp) ∧ (sp `isNaivelyStrongerThan` sp')
  ==>
  (secure joana pr impl sp')
 where isNaivelyStrongerThan = isNaivelyStrongerThanFor pr
\end{code}


%if False
unfortunately, naively checking this using QuickCheck is inefficient,

Instead, we define the enumeration of all "weakenings" of a given ifc specification:
"weakings pr sp" enumerates all weakenings of sp, i.e. all specifications sp' such that
     sp `isNaivelyStrongerThanFor pr` sp'
\begin{code}
weakenings :: (Ord d, Ord p, Enum d, Bounded d) => Component p -> Specification p d -> [Specification p d]
\end{code}
\begin{code}
weakenings pr@(Component { input, output }) sp@(Specification { includes }) =
   [ sp { includes = \p -> fromJust $ lookup p choice } | choice <- choices ]
  where choices = chooseOneEach $    [(i, [d | d <- toList $ powerset $ fromList allValues, d ⊇ includes i]) | i <- toList $  input]
                                  ++ [(o, [d | d <- toList $ powerset $ fromList allValues, d ⊆ includes o]) | o <- toList $ output]

        chooseOneEach :: [(a,[b])] -> [[(a,b)]]
        chooseOneEach choices = fmap (zip as) $ sequence bss
          where as  = fmap fst choices
                bss = fmap snd choices
\end{code}

Then, we check the following three properties:
\begin{code}
weakeningsAreWeaker :: (Enum d, Bounded d, Ord d, Ord p) => Component p -> Specification p d -> Bool
weakeningsAreWeaker pr sp = (∀) (weakenings pr sp) (\sp' -> sp `isNaivelyStrongerThan` sp')
    where isNaivelyStrongerThan = isNaivelyStrongerThanFor pr
\end{code}

\begin{code}
weakerAreWeakenings :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Specification p d -> Specification p d -> Property
weakerAreWeakenings pr sp sp' =
     sp `isNaivelyStrongerThan` sp'
 ==> (∃) (weakenings pr sp ) (\prw ->  (show $ prw) == (show $ sp')) -- TODO: dont use hacky string-comparison
    where isNaivelyStrongerThan = isNaivelyStrongerThanFor pr

\end{code}

If impl fullfills the ifc requirement sp, then also all weakenings sp' of sp do
\begin{code}
weakeningsAreSafe :: (Enum d, Bounded d, Ord d, Ord p) => Component p -> Implementation p -> Specification p d -> Property
weakeningsAreSafe pr impl sp = secure joana pr impl sp ==>
  (∀) (weakenings pr sp) (\sp' -> secure joana pr impl sp')
\end{code}
%endif


Criteria using Relabelings
---------------------------

Now we try define a criterion which allows us to re-use existing non-interference proofs by employing *relabelings*.
Assume that we have

* an existing ifc specification `sp`  in terms of a set `d`  (of datasets)
* proven `impl` secure, for example by establishing all verification conditions derived by the interpretation `joana`
 (or interpretation `greiner`  which, by property `joanaIsGreiner`, is equivalent)

Assume moreover, that  we have

  * a  new    ifc specification `sp'` in terms of a set `d'` (of datasets)
  * a  "relabeling" `r`

We are looking for a criterion `isConsistentRelabelingFor` such that when  `isConsistentRelabelingFor pr r sp sp'`
for some relabeling `r`, then it is guaranteed that impl is also secure wrt. `sp'`.


Relabelings - a wrong Definition
---------------------------------

In a first attempt, let us define a relabeling to be a function `g0` which interprets datasets

     ds' ∈ d' in terms of sets of datasets ds ⊆ d

Of course, simpler relabelings that interprets datasets

     ds' ∈ d' in terms of         datasets ds ∈ d

are a special case of this notion of "relabeling".

The following definition of `isConsistentRelabelingFor` checks that after restating `sp'` in terms of datasets `d` (by appling relabeling `g0`),
the resulting ifc specification is "naively weaker" than `sp`.

\begin{code}
isConsistentRelabelingFor :: forall d d' p. Ord d => Component p -> (d' -> Set d)  -> Specification p d -> Specification p d' -> Bool
isConsistentRelabelingFor pr g0 sp sp' =  sp `isNaivelyStrongerThan` (sp' `relabeledUsing` g0)
    where isNaivelyStrongerThan = isNaivelyStrongerThanFor pr
\end{code}

with the application of a relabeling defined as:
\begin{code}
relabeledUsing :: forall p d d'. (Ord d) =>  Specification p d' -> (d' -> Set d) -> Specification p d
sp' `relabeledUsing` g0 = Specification {
    includes = \p -> g (includes sp' p),
    datasets = g (datasets sp')
   }
  where g :: Set d' -> Set d
        g ds' = (⋃) [ g0 d' | d' <- toList ds']

\end{code}


Unfortunately, this criterion is neither sound nor complete, i.e.:

Neither does the following property hold ...
\begin{code}
existsConsistentRelabelingIsJustified ::  (Ord p, Ord d, Ord d') => Component p -> Implementation p -> Specification p d -> Specification p d' -> Property
existsConsistentRelabelingIsJustified pr impl sp sp' =
       (
          (secure joana pr impl sp)
        ∧ (∃) relabelings (\g0 -> isConsistentRelabelingFor pr g0 sp sp')
       )
   ==>    (secure joana pr impl sp')

  where relabelings = setFunctionsBetween (datasets sp) (datasets sp')
\end{code}

.. nor does this:
\begin{code}
existsConsistentRelabelingIsComplete ::  (Ord p, Ord d, Ord d') => Component p -> Implementation p -> Specification p d  -> Specification p d' -> Property
existsConsistentRelabelingIsComplete pr impl sp sp' =
       (
          (secure joana pr impl sp)
        ∧ (secure joana pr impl sp')
       )
   ==>    (∃) relabelings (\g0 -> isConsistentRelabelingFor pr g0 sp sp')

  where relabelings = setFunctionsBetween (datasets sp) (datasets sp')
\end{code}


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
We now "reverse" the direcetion of relabelings, i.e., we define a relabeling to be a function `f0` which interprets datasets

     ds ∈ d in terms of sets of datasets ds' ⊆ d'

Of course, simpler relabelings that interprets datasets

     ds ∈ d in terms of         datasets ds' ∈ d'

are again a special case of this notion of "relabeling".

The analogous Definition of `isConsistentRelabelingFor` reads:

\begin{code}
isConsistentRelabelingRevFor :: forall d d' p. Ord d' => Component p -> (d -> Set d')  -> Specification p d -> Specification p d' -> Bool
isConsistentRelabelingRevFor pr f0 sp sp' =  (sp `relabeledRevUsing` f0) `isNaivelyStrongerThan` sp'
    where isNaivelyStrongerThan = isNaivelyStrongerThanFor pr
\end{code}

with the application of a relabeling defined as:
\begin{code}
relabeledRevUsing :: forall p d d'. (Ord d') =>  Specification p d -> (d -> Set d') -> Specification p d'
sp `relabeledRevUsing` f0 =  Specification {
    includes = \p -> f (includes sp p),
    datasets = f (datasets sp)
   }
  where f :: Set d -> Set d'
        f ds = (⋃) [ f0 d | d <- toList ds]
\end{code}


This criterion *is* Sound:
\begin{code}
existsConsistentRelabelingRevIsJustified ::  (Ord p, Ord d, Ord d') => Component p -> Implementation p -> Specification p d -> Specification p d' -> Property
existsConsistentRelabelingRevIsJustified pr impl sp sp' =
       (
          (secure joana pr impl sp)
        ∧ (∃) relabelings (\f0 -> isConsistentRelabelingRevFor pr f0 sp sp')
       )
   ==>    (secure joana pr impl sp')

  where relabelings = setFunctionsBetween (datasets sp') (datasets sp)
\end{code}

.. but not complete, i.e., the following property does *not* hold:
\begin{code}
existsConsistentRelabelingRevIsComplete ::  (Ord p, Ord d, Ord d') => Component p -> Implementation p -> Specification p d  -> Specification p d' -> Property
existsConsistentRelabelingRevIsComplete  pr impl sp sp' =
       (
          (secure joana pr impl sp )
        ∧ (secure joana pr impl sp')
       )
   ==>    (∃) relabelings (\f0 -> isConsistentRelabelingRevFor pr f0 sp sp')

  where relabelings = setFunctionsBetween (datasets sp') (datasets sp)
\end{code}


Note that i'm currently don't know how to directly justify the soundness of this criterion.
An indirect justifcation stems from the following property (see below for the definition of `isStrongerThan`).
\begin{code}
relabeleingsRevAreStrongerThan ::  (Ord p, Ord d, Ord d') => Set d' -> Component p -> Specification p d -> Bool
relabeleingsRevAreStrongerThan ds' pr sp =
   (∀) relabelings (\f0 -> sp `isStrongerThan` (sp `relabeledRevUsing` f0))
  where relabelings = setFunctionsBetween ds' (datasets sp)
        isStrongerThan = isStrongerThanFor pr
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
existsConsistentRelabelingFor :: forall d d' p. (Ord d, Ord d', Ord p) => Component p -> Specification p d ->  Specification p d' -> Bool
existsConsistentRelabelingFor pr sp sp' =  (∃) relabelings (\g0 -> isConsistentRelabelingFor pr g0 sp sp')
  where relabelings = setFunctionsBetween (datasets sp) (datasets sp')

existsConsistentRelabelingRevFor :: forall d d' p. (Ord d, Ord d', Ord p) => Component p -> Specification p d -> Specification p d' -> Bool
existsConsistentRelabelingRevFor pr sp sp' =  (∃) relabelings (\g0 -> isConsistentRelabelingRevFor pr g0 sp sp')
  where relabelings = setFunctionsBetween (datasets sp') (datasets sp)
\end{code}
%endif


A Criterion without relabelings
-------------------------------
I will now develop Option 3.

Note that the sound (but not complete) relabeling-based criterion ("Option 2.") suggest two method of proof-reuse, given `sp` and `sp'`:

 1. let the user specifiy a relabeling candidate f0, and check whether

        isConsistentRelabelingRevFor pr f0 sp sp'

    holds.
 2. Instead of letting the user specify the candidate, enumerate all possible relabeling candidates `f0` and check them.
    If `sp` is stated in terms of `n` datasets, and `sp'` in terms of `m` datasets, there are `2^(n+m)` possible candidates.

Is there a sound and criterion that avoids the need of relabelings, but is at least as complete?

Given two ifc specifications `sp` and `sp'`, i propose to define the notion

        sp isStrongerThan        sp'           which, unlike
        sp isNaivelyStrongerThan sp'

is defined even if `sp` is stated in terms of a set `d` (of datasets) *different* from `d'` (the set of dataterms in which `sp'` is stated).

We will then have the soundness property:
\begin{code}
isStrongerThanIsJustified ::  (Ord p, Ord d, Ord d') => Component p -> Implementation p -> Specification p d ->  Specification p d' -> Property
isStrongerThanIsJustified pr impl sp sp' =
       (
          (secure joana pr impl sp)
        ∧ (sp `isStrongerThan` sp')
       )
   ==>    (secure joana pr impl sp')
  where isStrongerThan = isStrongerThanFor pr
\end{code}

.. but still *not* the completeness Property:
\begin{code}
isStrongerThanIsComplete ::  (Ord p, Ord d, Ord d') => Component p -> Implementation p -> Specification p d  -> Specification p d' -> Property
isStrongerThanIsComplete pr impl sp sp' =
       (
          (secure joana pr impl sp)
        ∧ (secure joana pr impl sp')
       )
   ==>    (sp  `isStrongerThan` sp')
  where isStrongerThan = isStrongerThanFor pr
\end{code}

In fact, Option 3.  will turn out to be strictly "better" than Option 2.:

It will hold that:
\begin{code}
isStrongerThanBetterThanConsistentRelabelingRevFor ::  (Ord p, Ord d, Ord d') => Component p -> Specification p d  -> Specification p d' -> Property
isStrongerThanBetterThanConsistentRelabelingRevFor pr sp sp' =
          (∃) relabelings (\f0 -> isConsistentRelabelingRevFor pr f0 sp sp')
   ==>    (sp  `isStrongerThan` sp')
  where relabelings = setFunctionsBetween (datasets sp') (datasets sp)
        isStrongerThan = isStrongerThanFor pr
\end{code}


... but *not* that:
\begin{code}
consistentRelabelingRevForBetterThanIsStrongerThan ::  (Ord p, Ord d, Ord d') =>  Component p -> Specification p d  -> Specification p d' -> Property
consistentRelabelingRevForBetterThanIsStrongerThan pr sp sp' =
          (sp  `isStrongerThan` sp')
   ==>    (∃) relabelings (\f0 -> isConsistentRelabelingRevFor pr f0 sp sp')
  where relabelings = setFunctionsBetween (datasets sp') (datasets sp)
        isStrongerThan = isStrongerThanFor pr
\end{code}


Now the definition of `isStrongerThan`:

Given two different ifc specifications for the same procedure using possibly different datasets `d` and `d'`, i.e.:
given a procedure `pr`,  a specification `sp` of type `Specification p d` and a specification sp'` of type `Specification p d'`,
the ifc specification `sp` is called "stronger" than `sp'` iff
for all input parameters `i`, the set of output parameters `o` that

  * by specification `sp` include no more than those datasets included in `i`

is included in the set of output parameters `o` that

  * by specification *`sp'`* include no more than those datasets included in `i`

\begin{code}
isStrongerThanFor ::  (Ord p, Ord d, Ord d') => Component p -> Specification p d -> Specification p d' -> Bool
isStrongerThanFor pr@(Component { input, output}) sp@(Specification { includes = includes }) sp'@(Specification { includes = includes' })  =
  (∀) input  (\i ->
        fromList [ o | o <- toList output, includes  i ⊇ includes  o ]
        ⊆
        fromList [ o | o <- toList output, includes' i ⊇ includes' o ]
  )
\end{code}

Deriviation of this Criterion
-----------------------------

This Criterion can be derived from a few concept. Doing this,
we will assume a fixed set of input and output parameters. I.e.: any

    pr :: Component p

we consider in this section will have the same set `(input pr)` of input-,
and the same set `(output pr)` of output-parameters.

Concept I.:     One implementation of a procedure `pr`may have fewer flows than another:
\begin{code}
hasFewerFlowsThanFor ::  (Ord p) => Component p -> Implementation p -> Implementation p -> Bool
hasFewerFlowsThanFor pr impl impl' =
      (∀) (input pr) (\i -> influences impl i ⊆ influences impl' i )
\end{code}



Concept II.:  An ifc specification

    sp :: Specification p d

can be understood as an abstraction of all "implementations" that satisfy the specification.
Among all such "implementations", there is one that is "most-leaking" (one with the most flows),
i.e.: there is one implementation `γ(sp)` that is the least
upper bound (by the partial order `hasFewerFlowsThan`) of all such implementations.

It is given by:
\begin{code}
γ :: (Ord d, Ord p) => Component p -> Specification p d -> Implementation p
γ (Component { input, output }) (Specification { includes }) =
  Implementation { influences = \p ->
                if (p ∈ input) then fromList [ p' | p' <- toList output, includes p ⊇ includes p' ]
                               else fromList []
            }
\end{code}

We then say that a specification `sp` is stronger than a specification `sp'` if the most-leaking implementation of `sp`
has fewer flows than the most-leaking implementation of `sp'`:

    pr `isStrongerThan` pr' ⇔ (γ pr) `hasFewerFlowsThan` (γ pr')

Indeed, this is equivalent to the definition of `isStrongerThan` given above,
which is easily shown by unfolding the definition of `γ`.

\begin{code}
isStrongerThanIsHasFewerFlowsThan ::  (Ord p, Ord d, Ord d') => Component p -> Specification p d  -> Specification p d' -> Bool
isStrongerThanIsHasFewerFlowsThan pr sp sp' =
      (γ pr sp) `hasFewerFlowsThan` (γ pr sp')
  ⇔        sp  `isStrongerThan`          sp'
  where isStrongerThan = isStrongerThanFor pr
        hasFewerFlowsThan = hasFewerFlowsThanFor pr
\end{code}



Property `isStrongerThanIsJustified` says that `isStrongerThan` is a "sound" criterion for re-use of specifications.
But is it better than its relabeling counter-part `isConsistentRelabeling(Rev)For` ?!?!
At the minimum, it is not worse, in the following sense:
\begin{code}
isStrongerThanIsBetterThanIsNaivelyStrongerThan ::  (Ord p, Ord d) => Component p -> Specification p d  -> Specification p d -> Property
isStrongerThanIsBetterThanIsNaivelyStrongerThan pr sp sp' =
       sp  `isNaivelyStrongerThan` sp'
  ==>  sp  `isStrongerThan`        sp'
   where isNaivelyStrongerThan = isNaivelyStrongerThanFor pr
         isStrongerThan = isStrongerThanFor pr
\end{code}

To see that it is better note that in the paper example, the generic ifc-specification in terms of datasets "Time" and "Data" *is*
stronger than the ifc-specification in terms of "Consumptiondata", and hence we can re-use it!
See `isStrongerThanCriterionHoldsGetValue` from module `Instances.PaperExample.ExampleOne.Noninterference`.


Appendix
========

It follows a section with some non-essential considerations.

Given an implementation `impl`, we can derive, in some sense, it's strongest ifc specification `α(impl)`.
This is in some sense "the" strongest ifc specification that `impl` fullfills, and it's derived by simply labeling
all output parameter by the set of input parameters that influence it.
\begin{code}
α :: (Ord p) => Component p -> Implementation p -> Specification p p
α (Component { input, output }) (Implementation { influences }) = Specification {
      datasets = input ∪ output,
      includes = includes
    }
  where includes p
          | p ∈ input  = influences p
          | p ∈ output = fromList [p]
          | otherwise  = fromList [] -- TODO: require some wellformedness for procedures
\end{code}


`α` and `γ` are monotone:

\begin{code}
αIsMonotone :: (Ord p) => Component p -> Implementation p -> Implementation p -> Property
αIsMonotone pr impl impl' =
            impl  `hasFewerFlowsThan`       impl'
  ==> (α pr impl) `isStrongerThan`    (α pr impl')
  where isStrongerThan        = isStrongerThanFor pr
        hasFewerFlowsThan     = hasFewerFlowsThanFor pr
\end{code}



\begin{code}
γIsMonotone :: (Ord p, Ord d, Ord d') => Component p -> Specification p d -> Specification p d' -> Property
γIsMonotone pr sp sp' =
            sp  `isStrongerThan`          sp'
  ==> (γ pr sp) `hasFewerFlowsThan` (γ pr sp)
  where isStrongerThan        = isStrongerThanFor pr
        hasFewerFlowsThan     = hasFewerFlowsThanFor pr
\end{code}


\begin{code}
γIsMonotone' :: (Ord p, Ord d) => Component p -> Specification p d -> Specification p d  -> Property
γIsMonotone' pr sp sp' =
            sp  `isNaivelyStrongerThan`   sp'
  ==> (γ pr sp) `hasFewerFlowsThan` (γ pr sp)
  where isNaivelyStrongerThan = isNaivelyStrongerThanFor pr
        hasFewerFlowsThan     = hasFewerFlowsThanFor pr
\end{code}



Formally, `α` and `γ` form a Galois-Connection (this, though is a rather weak statement, since `α` does not work for general datasets `d`):
\begin{code}
galoisAlphaGamma :: forall p d. (Ord p, Ord d, Bounded d, Enum d) => Component p -> Implementation p -> Specification p d -> Bool
galoisAlphaGamma pr impl sp =
     (α pr impl) `isStrongerThan`     sp
  ⇔       impl  `hasFewerFlowsThan` (γ pr sp)
  where isStrongerThan        = isStrongerThanFor pr
        hasFewerFlowsThan     = hasFewerFlowsThanFor pr
\end{code}

Trvially, an implementation `impl` is secure wrt. `sp` iff it is weaker that it's strongtes ifc specification `α(impl)`:
\begin{code}
weakerThanIffSecure  :: forall p d. (Ord p, Ord d) => Component p -> Implementation p -> Specification p d -> Bool
weakerThanIffSecure pr impl sp =
       (secure joana pr impl sp)
   ⇔  (α pr impl) `isStrongerThan`     sp
   where  isStrongerThan        = isStrongerThanFor pr
\end{code}



One alternativ way to derive a  correct ifc-specification is to pick one of the specifications `β(impl)`:
\begin{code}
β :: forall p d. (Ord p, Ord d, Bounded d, Enum d) => Component p -> Implementation p -> [Specification p d]
β (Component { input, output }) (Implementation { influences }) = do
      start <- [0 .. size input]
      let includes :: p -> Set d = (M.!) $ M.fromList $ [
             (i, if (S.null $ influences i) then (∅) else  (fromList allValues) ∖ (fromList [d])) | (i,d) <- zip (rotate start $ toList input) (cycle (allValues :: [d]))
           ] ++ [
             (o, (⋂) [ includes i | i <- toList input, o ∈ influences i]) | o <- toList output
           ]
      return $ Specification {
        datasets = fromList allValues,
        includes = includes
    }

β' :: forall p d. (Ord p, Ord d, Bounded d, Enum d) => Component p -> Implementation p -> Specification p d
β' pr impl = last $ sortBy (comparing sumOfOutputDatasets) (β pr impl :: [Specification p d])
  where sumOfOutputDatasets sp = sum [ size $ includes sp o | o <- toList $ output pr ]

β'' :: forall p d. (Ord p, Ord d, Bounded d, Enum d) => Component p -> Implementation p -> Specification p d
β'' pr impl = last $                                       (β pr impl :: [Specification p d])


β''' :: forall p d. (Ord p, Ord d, Bounded d, Enum d) => Component p -> Implementation p -> Specification p d
β''' pr impl = last $ sortBy (comparing sumOfOutputDatasets) minimals
  where minimals = [ sp | sp <- sps, (∀) sps (\sp' -> (not $ sp' `isStrongerThan` sp) ∨ (sp `isStrongerThan` sp'))]
        sps = (β pr impl :: [Specification p d])
        isStrongerThan = isStrongerThanFor pr
        sumOfOutputDatasets sp = sum [ size $ includes sp o | o <- toList $ output pr ]
\end{code}

Unfortunataley, neither of these `β` form a galois connection with `γ`. It does *not* hold in genaral that:
\begin{code}
galoisBetaGamma :: forall p d. (Ord p, Ord d, Bounded d, Enum d) => Component p -> Implementation p -> Specification p d -> Bool
galoisBetaGamma pr impl sp =
     (β''' pr impl :: Specification p d) `isStrongerThan`     sp
  ⇔                               impl  `hasFewerFlowsThan` (γ pr sp)
  where isStrongerThan        = isStrongerThanFor pr
        hasFewerFlowsThan     = hasFewerFlowsThanFor pr
\end{code}




Every implementation `impl` does indeed fullfill the ifc-specification `α(impl)`:
\begin{code}
mostPreciseIsSecure :: (Ord p) => Component p -> Implementation p -> Bool
mostPreciseIsSecure pr impl = secure joana pr impl (α pr impl)
\end{code}

Every implementation `impl` is equal to the the most-leaking implementation of it's most-precise ifc-specification
\begin{code}
γMostPreciseIsMostPrecuse :: (Ord p) => Component p -> Implementation p ->  Bool
γMostPreciseIsMostPrecuse pr impl = impl `eqImpl` γ pr (α pr impl)
  where impl `eqImpl` impl' = (∀) (input pr) (\p -> influences impl p == influences impl' p)
\end{code}

An auxiliary property that demonstrate that the definitions above are all natural:

An implementation `impl` is secure wrt. `sp` iff it has fewer flows than the most-leaking implementation of `sp`.
\begin{code}
fewerFlowsIffSecure  :: forall p d. (Ord p, Ord d) => Component p -> Implementation p -> Specification p d -> Bool
fewerFlowsIffSecure pr impl sp =
       (secure joana pr impl sp)
   ⇔  (impl `hasFewerFlowsThan` (γ pr sp))
  where hasFewerFlowsThan = hasFewerFlowsThanFor pr
\end{code}


An alternative definition of γ
\begin{code}
γ' :: (Ord p, Ord d) => Component p -> Specification p d -> Implementation p
γ' procedure@(Component { input, output}) sp@(Specification { includes }) =
    Implementation { influences = \p ->
                if (p ∈ input) then output ∖ fromList [ p' | p' <- toList output, ds <- toList $ includes p',
                                                                                  not $ ds ∈ includes p ]
                               else fromList []
            }

γIsγ' :: (Show d, Show p, Enum p, Bounded p, Ord d, Ord p) => Component p -> Specification p d -> Bool
γIsγ' pr sp = (show $ γ pr sp) == (show $ γ' pr sp) -- TODO: dont use hacky string-comparison
\end{code}



Strongest Guarantee w.r.t. weakened Assumptions
-----------------------------------------------

A given ifc-specification is comprised of "assumptions" and "guarantees":

 * `includes` restricted to `input`   parameters specifies assumptions on the information content of `input` parameters
 * `includes` restricted to `output`  parameters specifies guarantees made by the program on the the information content of `output` parameters


A natural question to ask is what guarantees still hold given a "weakening of assumptions".

Based on the notion of `isNaivelyStrongerThan`, we define

    `sp makesWeakerAssumptionsThan sp'`

and

    `sp makesStrongerGuaranteesThan sp'`

to mean:
\begin{code}
makesWeakerAssumptionsThanFor ::  (Ord d) => Component p -> Specification p d -> Specification p d -> Bool
makesWeakerAssumptionsThanFor pr sp sp'  =
      (∀) (input pr)  (\i -> includes sp' i ⊇ includes sp i)

makesStrongerGuaranteesThanFor ::  (Ord d) => Component p -> Specification p d -> Specification p d -> Bool
makesStrongerGuaranteesThanFor pr sp sp'  =
      (∀) (output pr) (\o -> includes sp' o ⊆ includes sp o)
\end{code}

i.e. we have:
\begin{code}
weakerStongerIsNaively :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Specification p d -> Specification p d -> Bool
weakerStongerIsNaively  pr sp sp' =
      sp `isNaivelyStrongerThan` sp'
  ⇔ (sp `makesWeakerAssumptionsThan`  sp'
    ∧ sp `makesStrongerGuaranteesThan` sp' )

  where isNaivelyStrongerThan = isNaivelyStrongerThanFor pr
        makesWeakerAssumptionsThan  = makesWeakerAssumptionsThanFor  pr
        makesStrongerGuaranteesThan = makesStrongerGuaranteesThanFor pr
\end{code}


Then, the strongest guarantee still valid given a weakening `sp'` of assumptions wrt. original specification `sp` is obtained by considering the most-leaking-implementation of the original specification `sp`

\begin{code}
strongestValidGuarantee :: (Ord d, Bounded d, Enum d, Ord p) => Component p -> Specification p d -> Specification p d -> Specification p d
strongestValidGuarantee pr@(Component { input, output }) sp sp' = Specification {
    includes = \p -> if (p ∈ input) then
                       (includes sp' p)
                     else
                       (⋂) [ includes sp' i  | i <- toList input, p ∈ (influences mostLeaking i)],
    datasets = datasets sp
    }
  where mostLeaking = γ pr sp
\end{code}

This is indeed valid:

\begin{code}
strongestValidGuaranteeIsValid :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Implementation p -> Specification p d -> Specification p d -> Property
strongestValidGuaranteeIsValid pr impl sp sp' =
      secure joana pr impl sp
  ==>
      secure joana pr impl $ strongestValidGuarantee pr sp sp'
\end{code}

Note that it is *not "extensive" in the following sense:

\begin{code}
strongestValidGuaranteeIsExtensive :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Specification  p d -> Specification p d -> Property
strongestValidGuaranteeIsExtensive pr sp sp' =
       sp' `makesWeakerAssumptionsThan` sp
  ==>
       sp `makesStrongerGuaranteesThan` (strongestValidGuarantee pr sp sp')
  where makesWeakerAssumptionsThan  = makesWeakerAssumptionsThanFor  pr
        makesStrongerGuaranteesThan = makesStrongerGuaranteesThanFor pr
\end{code}

since `sp` may include datasets in one of it's output parameters that are included in none of its input parameters.

Once can, of course, define an "extensive" operator like this:


\begin{code}
strongestValidGuaranteeExtensive :: (Ord d, Bounded d, Enum d, Ord p) => Component p -> Specification p d -> Specification p d -> Specification p d
strongestValidGuaranteeExtensive pr@(Component { input, output }) sp sp' = Specification {
    includes = \p -> if (p ∈ input) then
                       (includes sp'' p) -- == includes sp' p
                     else
                       (includes sp'' p) ∩ (includes sp p),
    datasets = datasets sp -- == datasets sp''
    }
  where sp'' = strongestValidGuarantee pr sp sp'
\end{code}


such that both:

\begin{code}
strongestValidGuaranteeExtensiveIsExtensive :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Specification  p d -> Specification p d -> Property
strongestValidGuaranteeExtensiveIsExtensive pr sp sp' =
       sp' `makesWeakerAssumptionsThan` sp
  ==>
       sp  `makesStrongerGuaranteesThan` (strongestValidGuaranteeExtensive pr sp sp')
  where makesWeakerAssumptionsThan  = makesWeakerAssumptionsThanFor  pr
        makesStrongerGuaranteesThan = makesStrongerGuaranteesThanFor pr
\end{code}


and:

\begin{code}
strongestValidGuaranteeExtensiveIsValid :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Implementation p -> Specification  p d -> Specification p d -> Property
strongestValidGuaranteeExtensiveIsValid pr impl sp sp' =
      secure joana pr impl sp
  ==>
      secure joana pr impl $ strongestValidGuaranteeExtensive pr sp sp'
\end{code}


Both operator are idempotent:

\begin{code}
strongestValidGuaranteeExtensiveIsIdempotent :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Specification p d -> Specification p d -> Bool
strongestValidGuaranteeExtensiveIsIdempotent pr sp sp' =
      (show $ strongestValidGuaranteeExtensive pr sp                                            sp') ==
      (show $ strongestValidGuaranteeExtensive pr (strongestValidGuaranteeExtensive pr sp sp')  sp')      -- TODO: dont use hacky string-comparison
\end{code}

\begin{code}
strongestValidGuaranteeIsIdempotent :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Specification p d -> Specification p d -> Bool
strongestValidGuaranteeIsIdempotent pr sp sp' =
      (show $ strongestValidGuarantee pr sp                                   sp') ==
      (show $ strongestValidGuarantee pr (strongestValidGuarantee pr sp sp')  sp')      -- TODO: dont use hacky string-comparison
\end{code}

