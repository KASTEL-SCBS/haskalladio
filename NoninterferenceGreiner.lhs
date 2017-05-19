%if False
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module NoninterferenceGreiner where

import Noninterference.Util
import Noninterference.Component
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
key (Specification { includes, datasets }) =
    [ (lowhigh, (\p -> if (d ∈ includes p) then Low else High)) | d <- toList $ datasets ]
\end{code}

The approach for JOANA generates just one JOANA-Specification, using the reversed powerset-lattice of `d`.
\begin{code}
joana :: (Ord d, Ord p) => SpecificationInterpretation p d (Set d)
joana (Specification { includes, datasets }) = [((powerset datasets, (⊇)), includes )]
\end{code}

In this simplified model, a components implementation is abstractly defined by its information-flow
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
a component co is secure iff all its verification conditions hold.
\begin{code}
secure :: (Ord p) => SpecificationInterpretation p d l -> Component p -> Implementation p -> Specification p d -> Bool
secure interpretation co impl sp = (∀) (interpretation sp) (\condition -> holds co impl condition)
\end{code}

`joana` and `key` are equivalent! Otherwise, we couldn't use KeY and JOANA interchangably!!
\begin{code}
joanaIsKey :: (Ord d, Ord p) => Component p -> Implementation p -> Specification p d -> Bool
joanaIsKey co impl sp = secure joana co impl sp ⇔ secure key co impl sp
\end{code}


Specifically, a component being secure is characerized as:
\begin{code}
secureCharactization :: forall d p. (Ord d, Ord p) => Component p -> Implementation p -> Specification p d -> Bool
secureCharactization co@(Component { input, output }) impl@(Implementation { influences }) sp@(Specification { includes }) =
       (secure key co impl sp)
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

Given two different ifc specifications for the same component `co` using the *same* datasets `d`,
i.e.: given specifications `sp`, `sp'` of type `Specification p d`,
the ifc specification `sp` (for `co`) is called "naively stronger" than the specification `sp'` (for `co`) iff

    sp `(isNaivelyStrongerThanFor co)` sp'

as defined here:
\begin{code}
isNaivelyStrongerThanFor ::  (Ord d) => Component p -> Specification p d -> Specification p d -> Bool
isNaivelyStrongerThanFor co sp sp' =
      (∀) (input co)  (\i -> includes sp' i ⊇ includes sp i)
  ∧   (∀) (output co) (\o -> includes sp' o ⊆ includes sp o)
\end{code}

Secure implementations  remain secure if a specification is weakened:
\begin{code}
secureWeakeningsAreSecure :: (Enum d, Bounded d, Ord d, Ord p) => Component p -> Implementation p -> Specification p d -> Specification p d -> Property
secureWeakeningsAreSecure co impl sp sp' =
  (secure joana co impl sp) ∧ (sp `isNaivelyStrongerThan` sp')
  ==>
  (secure joana co impl sp')
 where isNaivelyStrongerThan = isNaivelyStrongerThanFor co
\end{code}


%if False
unfortunately, naively checking this using QuickCheck is inefficient,

Instead, we define the enumeration of all "weakenings" of a given ifc specification:
"weakings co sp" enumerates all weakenings of sp, i.e. all specifications sp' such that
     sp `isNaivelyStrongerThanFor co` sp'
\begin{code}
weakenings :: (Ord d, Ord p, Enum d, Bounded d) => Component p -> Specification p d -> [Specification p d]
\end{code}
\begin{code}
weakenings co@(Component { input, output }) sp@(Specification { includes }) =
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
weakeningsAreWeaker co sp = (∀) (weakenings co sp) (\sp' -> sp `isNaivelyStrongerThan` sp')
    where isNaivelyStrongerThan = isNaivelyStrongerThanFor co
\end{code}

\begin{code}
weakerAreWeakenings :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Specification p d -> Specification p d -> Property
weakerAreWeakenings co sp sp' =
     sp `isNaivelyStrongerThan` sp'
 ==> (∃) (weakenings co sp ) (\spw ->  (show $ spw) == (show $ sp')) -- TODO: dont use hacky string-comparison
    where isNaivelyStrongerThan = isNaivelyStrongerThanFor co

\end{code}

If impl fullfills the ifc requirement sp, then also all weakenings sp' of sp do
\begin{code}
weakeningsAreSafe :: (Enum d, Bounded d, Ord d, Ord p) => Component p -> Implementation p -> Specification p d -> Property
weakeningsAreSafe co impl sp = secure joana co impl sp ==>
  (∀) (weakenings co sp) (\sp' -> secure joana co impl sp')
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

We are looking for a criterion `isConsistentRelabelingFor` such that when  `isConsistentRelabelingFor co r sp sp'`
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
isConsistentRelabelingFor co g0 sp sp' =  sp `isNaivelyStrongerThan` (sp' `relabeledUsing` g0)
    where isNaivelyStrongerThan = isNaivelyStrongerThanFor co
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
existsConsistentRelabelingIsJustified co impl sp sp' =
       (
          (secure joana co impl sp)
        ∧ (∃) relabelings (\g0 -> isConsistentRelabelingFor co g0 sp sp')
       )
   ==>    (secure joana co impl sp')

  where relabelings = setFunctionsBetween (datasets sp) (datasets sp')
\end{code}

.. nor does this:
\begin{code}
existsConsistentRelabelingIsComplete ::  (Ord p, Ord d, Ord d') => Component p -> Implementation p -> Specification p d  -> Specification p d' -> Property
existsConsistentRelabelingIsComplete co impl sp sp' =
       (
          (secure joana co impl sp)
        ∧ (secure joana co impl sp')
       )
   ==>    (∃) relabelings (\g0 -> isConsistentRelabelingFor co g0 sp sp')

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
isConsistentRelabelingRevFor co f0 sp sp' =  (sp `relabeledRevUsing` f0) `isNaivelyStrongerThan` sp'
    where isNaivelyStrongerThan = isNaivelyStrongerThanFor co
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
existsConsistentRelabelingRevIsJustified co impl sp sp' =
       (
          (secure joana co impl sp)
        ∧ (∃) relabelings (\f0 -> isConsistentRelabelingRevFor co f0 sp sp')
       )
   ==>    (secure joana co impl sp')

  where relabelings = setFunctionsBetween (datasets sp') (datasets sp)
\end{code}

.. but not complete, i.e., the following property does *not* hold:
\begin{code}
existsConsistentRelabelingRevIsComplete ::  (Ord p, Ord d, Ord d') => Component p -> Implementation p -> Specification p d  -> Specification p d' -> Property
existsConsistentRelabelingRevIsComplete  co impl sp sp' =
       (
          (secure joana co impl sp )
        ∧ (secure joana co impl sp')
       )
   ==>    (∃) relabelings (\f0 -> isConsistentRelabelingRevFor co f0 sp sp')

  where relabelings = setFunctionsBetween (datasets sp') (datasets sp)
\end{code}


Note that i'm currently don't know how to directly justify the soundness of this criterion.
An indirect justifcation stems from the following property (see below for the definition of `isStrongerThan`).
\begin{code}
relabeleingsRevAreStrongerThan ::  (Ord p, Ord d, Ord d') => Set d' -> Component p -> Specification p d -> Bool
relabeleingsRevAreStrongerThan ds' co sp =
   (∀) relabelings (\f0 -> sp `isStrongerThan` (sp `relabeledRevUsing` f0))
  where relabelings = setFunctionsBetween ds' (datasets sp)
        isStrongerThan = isStrongerThanFor co
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
existsConsistentRelabelingFor co sp sp' =  (∃) relabelings (\g0 -> isConsistentRelabelingFor co g0 sp sp')
  where relabelings = setFunctionsBetween (datasets sp) (datasets sp')

existsConsistentRelabelingRevFor :: forall d d' p. (Ord d, Ord d', Ord p) => Component p -> Specification p d -> Specification p d' -> Bool
existsConsistentRelabelingRevFor co sp sp' =  (∃) relabelings (\g0 -> isConsistentRelabelingRevFor co g0 sp sp')
  where relabelings = setFunctionsBetween (datasets sp') (datasets sp)
\end{code}
%endif


A Criterion without relabelings
-------------------------------
I will now develop Option 3.

Note that the sound (but not complete) relabeling-based criterion ("Option 2.") suggest two method of proof-reuse, given `sp` and `sp'`:

 1. let the user specifiy a relabeling candidate f0, and check whether

        isConsistentRelabelingRevFor co f0 sp sp'

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
isStrongerThanIsJustified co impl sp sp' =
       (
          (secure joana co impl sp)
        ∧ (sp `isStrongerThan` sp')
       )
   ==>    (secure joana co impl sp')
  where isStrongerThan = isStrongerThanFor co
\end{code}

.. but still *not* the completeness Property:
\begin{code}
isStrongerThanIsComplete ::  (Ord p, Ord d, Ord d') => Component p -> Implementation p -> Specification p d  -> Specification p d' -> Property
isStrongerThanIsComplete co impl sp sp' =
       (
          (secure joana co impl sp)
        ∧ (secure joana co impl sp')
       )
   ==>    (sp  `isStrongerThan` sp')
  where isStrongerThan = isStrongerThanFor co
\end{code}

In fact, Option 3.  will turn out to be strictly "better" than Option 2.:

It will hold that:
\begin{code}
isStrongerThanBetterThanConsistentRelabelingRevFor ::  (Ord p, Ord d, Ord d') => Component p -> Specification p d  -> Specification p d' -> Property
isStrongerThanBetterThanConsistentRelabelingRevFor co sp sp' =
          (∃) relabelings (\f0 -> isConsistentRelabelingRevFor co f0 sp sp')
   ==>    (sp  `isStrongerThan` sp')
  where relabelings = setFunctionsBetween (datasets sp') (datasets sp)
        isStrongerThan = isStrongerThanFor co
\end{code}


... but *not* that:
\begin{code}
consistentRelabelingRevForBetterThanIsStrongerThan ::  (Ord p, Ord d, Ord d') =>  Component p -> Specification p d  -> Specification p d' -> Property
consistentRelabelingRevForBetterThanIsStrongerThan co sp sp' =
          (sp  `isStrongerThan` sp')
   ==>    (∃) relabelings (\f0 -> isConsistentRelabelingRevFor co f0 sp sp')
  where relabelings = setFunctionsBetween (datasets sp') (datasets sp)
        isStrongerThan = isStrongerThanFor co
\end{code}


Now the definition of `isStrongerThan`:

Given two different ifc specifications for the same component using possibly different datasets `d` and `d'`, i.e.:
given a component `co`,  a specification `sp` of type `Specification p d` and a specification sp'` of type `Specification p d'`,
the ifc specification `sp` is called "stronger" than `sp'` iff
for all input parameters `i`, the set of output parameters `o` that

  * by specification `sp` include no more than those datasets included in `i`

is included in the set of output parameters `o` that

  * by specification *`sp'`* include no more than those datasets included in `i`

\begin{code}
isStrongerThanFor ::  (Ord p, Ord d, Ord d') => Component p -> Specification p d -> Specification p d' -> Bool
isStrongerThanFor co@(Component { input, output}) sp@(Specification { includes = includes }) sp'@(Specification { includes = includes' })  =
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

    co :: Component p

we consider in this section will have the same set `(input co)` of input-,
and the same set `(output co)` of output-parameters.

Concept I.:     One implementation of a component `co`may have fewer flows than another:
\begin{code}
hasFewerFlowsThanFor ::  (Ord p) => Component p -> Implementation p -> Implementation p -> Bool
hasFewerFlowsThanFor co impl impl' =
      (∀) (input co) (\i -> influences impl i ⊆ influences impl' i )
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

    co `isStrongerThan` co' ⇔ (γ co) `hasFewerFlowsThan` (γ co')

Indeed, this is equivalent to the definition of `isStrongerThan` given above,
which is easily shown by unfolding the definition of `γ`.

\begin{code}
isStrongerThanIsHasFewerFlowsThan ::  (Ord p, Ord d, Ord d') => Component p -> Specification p d  -> Specification p d' -> Bool
isStrongerThanIsHasFewerFlowsThan co sp sp' =
      (γ co sp) `hasFewerFlowsThan` (γ co sp')
  ⇔        sp  `isStrongerThan`          sp'
  where isStrongerThan = isStrongerThanFor co
        hasFewerFlowsThan = hasFewerFlowsThanFor co
\end{code}



Property `isStrongerThanIsJustified` says that `isStrongerThan` is a "sound" criterion for re-use of specifications.
But is it better than its relabeling counter-part `isConsistentRelabeling(Rev)For` ?!?!
At the minimum, it is not worse, in the following sense:
\begin{code}
isStrongerThanIsBetterThanIsNaivelyStrongerThan ::  (Ord p, Ord d) => Component p -> Specification p d  -> Specification p d -> Property
isStrongerThanIsBetterThanIsNaivelyStrongerThan co sp sp' =
       sp  `isNaivelyStrongerThan` sp'
  ==>  sp  `isStrongerThan`        sp'
   where isNaivelyStrongerThan = isNaivelyStrongerThanFor co
         isStrongerThan = isStrongerThanFor co
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
          | otherwise  = fromList [] -- TODO: require some wellformedness for components
\end{code}


`α` and `γ` are monotone:

\begin{code}
αIsMonotone :: (Ord p) => Component p -> Implementation p -> Implementation p -> Property
αIsMonotone co impl impl' =
            impl  `hasFewerFlowsThan`       impl'
  ==> (α co impl) `isStrongerThan`    (α co impl')
  where isStrongerThan        = isStrongerThanFor co
        hasFewerFlowsThan     = hasFewerFlowsThanFor co
\end{code}



\begin{code}
γIsMonotone :: (Ord p, Ord d, Ord d') => Component p -> Specification p d -> Specification p d' -> Property
γIsMonotone co sp sp' =
            sp  `isStrongerThan`          sp'
  ==> (γ co sp) `hasFewerFlowsThan` (γ co sp)
  where isStrongerThan        = isStrongerThanFor co
        hasFewerFlowsThan     = hasFewerFlowsThanFor co
\end{code}


\begin{code}
γIsMonotone' :: (Ord p, Ord d) => Component p -> Specification p d -> Specification p d  -> Property
γIsMonotone' co sp sp' =
            sp  `isNaivelyStrongerThan`   sp'
  ==> (γ co sp) `hasFewerFlowsThan` (γ co sp)
  where isNaivelyStrongerThan = isNaivelyStrongerThanFor co
        hasFewerFlowsThan     = hasFewerFlowsThanFor co
\end{code}



Formally, `α` and `γ` form a Galois-Connection (this, though is a rather weak statement, since `α` does not work for general datasets `d`):
\begin{code}
galoisAlphaGamma :: forall p d. (Ord p, Ord d, Bounded d, Enum d) => Component p -> Implementation p -> Specification p d -> Bool
galoisAlphaGamma co impl sp =
     (α co impl) `isStrongerThan`     sp
  ⇔       impl  `hasFewerFlowsThan` (γ co sp)
  where isStrongerThan        = isStrongerThanFor co
        hasFewerFlowsThan     = hasFewerFlowsThanFor co
\end{code}

Trvially, an implementation `impl` is secure wrt. `sp` iff it is weaker that it's strongtes ifc specification `α(impl)`:
\begin{code}
weakerThanIffSecure  :: forall p d. (Ord p, Ord d) => Component p -> Implementation p -> Specification p d -> Bool
weakerThanIffSecure co impl sp =
       (secure joana co impl sp)
   ⇔  (α co impl) `isStrongerThan`     sp
   where  isStrongerThan        = isStrongerThanFor co
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
β' co impl = last $ sortBy (comparing sumOfOutputDatasets) (β co impl :: [Specification p d])
  where sumOfOutputDatasets sp = sum [ size $ includes sp o | o <- toList $ output co ]

β'' :: forall p d. (Ord p, Ord d, Bounded d, Enum d) => Component p -> Implementation p -> Specification p d
β'' co impl = last $                                       (β co impl :: [Specification p d])


β''' :: forall p d. (Ord p, Ord d, Bounded d, Enum d) => Component p -> Implementation p -> Specification p d
β''' co impl = last $ sortBy (comparing sumOfOutputDatasets) minimals
  where minimals = [ sp | sp <- sps, (∀) sps (\sp' -> (not $ sp' `isStrongerThan` sp) ∨ (sp `isStrongerThan` sp'))]
        sps = (β co impl :: [Specification p d])
        isStrongerThan = isStrongerThanFor co
        sumOfOutputDatasets sp = sum [ size $ includes sp o | o <- toList $ output co ]
\end{code}

Unfortunataley, neither of these `β` form a galois connection with `γ`. It does *not* hold in genaral that:
\begin{code}
galoisBetaGamma :: forall p d. (Ord p, Ord d, Bounded d, Enum d) => Component p -> Implementation p -> Specification p d -> Bool
galoisBetaGamma co impl sp =
     (β''' co impl :: Specification p d) `isStrongerThan`     sp
  ⇔                               impl  `hasFewerFlowsThan` (γ co sp)
  where isStrongerThan        = isStrongerThanFor co
        hasFewerFlowsThan     = hasFewerFlowsThanFor co
\end{code}




Every implementation `impl` does indeed fullfill the ifc-specification `α(impl)`:
\begin{code}
mostPreciseIsSecure :: (Ord p) => Component p -> Implementation p -> Bool
mostPreciseIsSecure co impl = secure joana co impl (α co impl)
\end{code}

Every implementation `impl` is equal to the the most-leaking implementation of it's most-precise ifc-specification
\begin{code}
γMostPreciseIsMostPrecuse :: (Ord p) => Component p -> Implementation p ->  Bool
γMostPreciseIsMostPrecuse co impl = impl `eqImpl` γ co (α co impl)
  where impl `eqImpl` impl' = (∀) (input co) (\p -> influences impl p == influences impl' p)
\end{code}

An auxiliary property that demonstrate that the definitions above are all natural:

An implementation `impl` is secure wrt. `sp` iff it has fewer flows than the most-leaking implementation of `sp`.
\begin{code}
fewerFlowsIffSecure  :: forall p d. (Ord p, Ord d) => Component p -> Implementation p -> Specification p d -> Bool
fewerFlowsIffSecure co impl sp =
       (secure joana co impl sp)
   ⇔  (impl `hasFewerFlowsThan` (γ co sp))
  where hasFewerFlowsThan = hasFewerFlowsThanFor co
\end{code}


An alternative definition of γ
\begin{code}
γ' :: (Ord p, Ord d) => Component p -> Specification p d -> Implementation p
γ' component@(Component { input, output}) sp@(Specification { includes }) =
    Implementation { influences = \p ->
                if (p ∈ input) then output ∖ fromList [ p' | p' <- toList output, ds <- toList $ includes p',
                                                                                  not $ ds ∈ includes p ]
                               else fromList []
            }

γIsγ' :: (Show d, Show p, Enum p, Bounded p, Ord d, Ord p) => Component p -> Specification p d -> Bool
γIsγ' co sp = (show $ γ co sp) == (show $ γ' co sp) -- TODO: dont use hacky string-comparison
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
makesWeakerAssumptionsThanFor co sp sp'  =
      (∀) (input co)  (\i -> includes sp' i ⊇ includes sp i)

makesStrongerGuaranteesThanFor ::  (Ord d) => Component p -> Specification p d -> Specification p d -> Bool
makesStrongerGuaranteesThanFor co sp sp'  =
      (∀) (output co) (\o -> includes sp' o ⊆ includes sp o)
\end{code}

i.e. we have:
\begin{code}
weakerStongerIsNaively :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Specification p d -> Specification p d -> Bool
weakerStongerIsNaively  co sp sp' =
      sp `isNaivelyStrongerThan` sp'
  ⇔ (sp `makesWeakerAssumptionsThan`  sp'
    ∧ sp `makesStrongerGuaranteesThan` sp' )

  where isNaivelyStrongerThan = isNaivelyStrongerThanFor co
        makesWeakerAssumptionsThan  = makesWeakerAssumptionsThanFor  co
        makesStrongerGuaranteesThan = makesStrongerGuaranteesThanFor co
\end{code}


Then, the strongest guarantee still valid given a weakening `sp'` of assumptions wrt. original specification `sp` is obtained by considering the most-leaking-implementation of the original specification `sp`

\begin{code}
strongestValidGuarantee :: (Ord d, Ord p, Ord d', Bounded d', Enum d') => Component p -> Specification p d -> Specification p d' -> Specification p d'
strongestValidGuarantee co@(Component { input, output }) sp sp' = Specification {
    includes = \p -> if (p ∈ input) then
                       (includes sp' p)
                     else
                       (⋂) [ includes sp' i  | i <- toList input, p ∈ (influences mostLeaking i)],
    datasets = datasets sp'
    }
  where mostLeaking = γ co sp
\end{code}

This is indeed valid:

\begin{code}
strongestValidGuaranteeIsValid :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Implementation p -> Specification p d -> Specification p d -> Property
strongestValidGuaranteeIsValid co impl sp sp' =
      secure joana co impl sp
  ==>
      secure joana co impl $ strongestValidGuarantee co sp sp'
\end{code}

Note that it is *not "extensive" in the following sense:

\begin{code}
strongestValidGuaranteeIsExtensive :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Specification  p d -> Specification p d -> Property
strongestValidGuaranteeIsExtensive co sp sp' =
       sp' `makesWeakerAssumptionsThan` sp
  ==>
       sp `makesStrongerGuaranteesThan` (strongestValidGuarantee co sp sp')
  where makesWeakerAssumptionsThan  = makesWeakerAssumptionsThanFor  co
        makesStrongerGuaranteesThan = makesStrongerGuaranteesThanFor co
\end{code}

since `sp` may include datasets in one of it's output parameters that are included in none of its input parameters.

Once can, of course, define an "extensive" operator like this:


\begin{code}
strongestValidGuaranteeExtensive :: (Ord d, Bounded d, Enum d, Ord p) => Component p -> Specification p d -> Specification p d -> Specification p d
strongestValidGuaranteeExtensive co@(Component { input, output }) sp sp' = Specification {
    includes = \p -> if (p ∈ input) then
                       (includes sp'' p) -- == includes sp' p
                     else
                       (includes sp'' p) ∩ (includes sp p),
    datasets = datasets sp -- == datasets sp''
    }
  where sp'' = strongestValidGuarantee co sp sp'
\end{code}


such that both:

\begin{code}
strongestValidGuaranteeExtensiveIsExtensive :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Specification  p d -> Specification p d -> Property
strongestValidGuaranteeExtensiveIsExtensive co sp sp' =
       sp' `makesWeakerAssumptionsThan` sp
  ==>
       sp  `makesStrongerGuaranteesThan` (strongestValidGuaranteeExtensive co sp sp')
  where makesWeakerAssumptionsThan  = makesWeakerAssumptionsThanFor  co
        makesStrongerGuaranteesThan = makesStrongerGuaranteesThanFor co
\end{code}


and:

\begin{code}
strongestValidGuaranteeExtensiveIsValid :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Implementation p -> Specification  p d -> Specification p d -> Property
strongestValidGuaranteeExtensiveIsValid co impl sp sp' =
      secure joana co impl sp
  ==>
      secure joana co impl $ strongestValidGuaranteeExtensive co sp sp'
\end{code}


Both operator are idempotent:

\begin{code}
strongestValidGuaranteeExtensiveIsIdempotent :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Specification p d -> Specification p d -> Bool
strongestValidGuaranteeExtensiveIsIdempotent co sp sp' =
      (show $ strongestValidGuaranteeExtensive co sp                                            sp') ==
      (show $ strongestValidGuaranteeExtensive co (strongestValidGuaranteeExtensive co sp sp')  sp')      -- TODO: dont use hacky string-comparison
\end{code}

\begin{code}
strongestValidGuaranteeIsIdempotent :: (Enum d, Enum p, Bounded p, Bounded d, Show d, Show p, Ord d, Ord p) => Component p -> Specification p d -> Specification p d -> Bool
strongestValidGuaranteeIsIdempotent co sp sp' =
      (show $ strongestValidGuarantee co sp                                   sp') ==
      (show $ strongestValidGuarantee co (strongestValidGuarantee co sp sp')  sp')      -- TODO: dont use hacky string-comparison
\end{code}

