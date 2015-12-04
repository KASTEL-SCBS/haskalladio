{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Noninterference where




import Prelude as P

import Algebra.Lattice
import Unicode
-- import Misc

import Data.Set as S
import Data.Set.Unicode
import Data.Maybe (fromJust)

import qualified Data.Map as M

import Control.Monad.Writer
import Control.Monad

import Test.QuickCheck hiding (output)

type OrderedSet p = (Set p, p -> p -> Bool)

class Enumerable a where
  allValues :: [a]

instance (Bounded a, Enum a) => Enumerable a where
  allValues = [minBound..]



-- A Procedure is described by
-- * influences p: the set of output-parameters p may influence
-- * includes p: its parameter classifcation, as set of datasets
--
-- "includes" hence implicitly specifies an ifc requirement, which may or may not be fullfilled by "influences".
data Procedure p d = Procedure {
    input :: Set p,
    output :: Set p,
    includes :: p -> (Set d),
    influences :: p ->  (Set p)
  }


powerset :: Ord a => Set a -> Set (Set a)
powerset = S.fromList . fmap S.fromList . listPowerset . S.toList

listPowerset :: [a] -> [[a]]
listPowerset = filterM (const [True, False])

-- powersetorder :: Ord a => Set a -> OrderedSet (Set a)
-- powersetorder a = (powerset a, (⊆))

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

data CompletelyDifferentDatasets = SomeData
                                 | OtherData
              deriving (Show, Eq, Ord, Enum, Bounded)


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

showMapFun :: (Show a, Show b, Enumerable a, Ord a, Ord b) => (a -> b) -> String
showMapFun f = show $ fromList $ [(x, f x) | x <- allValues]


instance (Show p, Show d, Ord p, Ord d, Enumerable p) =>  Show (Procedure p d) where
  show (Procedure { input, output, includes, influences}) =
    "Procedure { input = " ++ (show input) ++ ", output = " ++ (show output) ++ ", includes = (M.!) $ M." ++ (showMapFun includes) ++ ", influences = (M.!) $ M." ++ (showMapFun influences) ++ " }"

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


data SpecificationPair p d d' = SpecificationPair (Procedure p d) (Procedure p d') deriving Show
instance (Enumerable d, Ord d, Enumerable d', Ord d') => Arbitrary (SpecificationPair Parameter d d') where
  arbitrary = do
      pr  <- arbitrary
      pr' <- arbitrary
      return $ SpecificationPair pr (pr { includes = includes pr' })


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


data LowHigh = Low
             | High
             deriving (Show, Eq, Ord, Enum, Bounded)


lowhigh = (S.fromList [Low, High], (⊑))
  where Low  ⊑ Low  = True
        Low  ⊑ High = True
        High ⊑ High = True
        High ⊑ Low  = False

datasets :: (Ord d, Ord p) => Procedure p d -> Set d
datasets (Procedure { input, output, includes, influences}) = fromList [ d | p <- toList $ output ∪ input, d <- toList $ includes p]


greiner :: (Ord d, Ord p) => Procedure p d -> [(p -> LowHigh, OrderedSet LowHigh)]
greiner pr@(Procedure { input, output, includes, influences}) =
    [ ((\p -> if (d ∈ includes p) then High else Low), lowhigh) | d <- toList $ datasets pr]


greiner2 :: (Ord d, Ord p) => Procedure p d -> [(p -> LowHigh, OrderedSet LowHigh)]
greiner2 pr@(Procedure { input, output, includes, influences}) =
    [ ((\p -> if (d ∈ includes p) then Low else High), lowhigh) | d <- toList $ datasets pr]


hecker :: (Ord d, Ord p) => Procedure p d -> (p -> Set d, OrderedSet (Set d))
hecker pr@(Procedure { input, output, includes, influences}) = (includes,  (powerset (datasets pr), (⊆)) )

                                                                                                    
hecker2 :: (Ord d, Ord p) => Procedure p d -> (p -> Set d, OrderedSet (Set d))
hecker2 pr@(Procedure { input, output, includes, influences}) = (includes,  (powerset (datasets pr), (⊇)) )
                                                                                                    

  
secure :: (Ord p) => Procedure p d -> (p -> l) -> OrderedSet l -> Bool
secure (Procedure { input, output, includes, influences}) classifiedAs (l,(⊑)) =
    (∀) input (\(i :: p) ->   (∀) output (\(o :: p) ->
          (o ∈ (influences i)) → (classifiedAs i ⊑ classifiedAs o)
    ))


hIsG :: Procedure Parameter Datasets -> Bool
hIsG = heckerIsGreiner

heckerIsGreiner :: (Ord d, Ord p) => Procedure p d -> Bool
heckerIsGreiner p = 
       and [ secure p classifiedAsGreiner latticeGreiner | (classifiedAsGreiner, latticeGreiner) <- greiner p]
    == secure p classifiedAsHecker latticeHecker where     (classifiedAsHecker,  latticeHecker)   = hecker p

-- heckerOf  :: Procedure Parameter Datasets -> Bool
heckerOf  p = secure p classifiedAsHecker latticeHecker where     (classifiedAsHecker,  latticeHecker)   = hecker p

-- greinerOf :: (Ord d, Ord p) => Procedure p d -> Bool
greinerOf p = and [ secure p classifiedAsGreiner latticeGreiner | (classifiedAsGreiner, latticeGreiner) <- greiner p]

h2IsG2 :: Procedure Parameter Datasets -> Bool
h2IsG2 = hecker2IsGreiner2

hecker2IsGreiner2 :: (Ord d, Ord p) => Procedure p d -> Bool
hecker2IsGreiner2 p = 
       and [ secure p classifiedAsGreiner2 latticeGreiner2 | (classifiedAsGreiner2, latticeGreiner2) <- greiner2 p]
    == secure p classifiedAsHecker2 latticeHecker2 where     (classifiedAsHecker2,  latticeHecker2)   = hecker2 p

hecker2Of  :: Procedure Parameter Datasets -> Bool
hecker2Of  p = secure p classifiedAsHecker2 latticeHecker2 where     (classifiedAsHecker2,  latticeHecker2)   = hecker2 p

greiner2Of :: (Ord d, Ord p) => Procedure p d -> Bool
greiner2Of p = and [ secure p classifiedAsGreiner2 latticeGreiner2 | (classifiedAsGreiner2, latticeGreiner2) <- greiner2 p]



mostPreciseIsSecure :: (Ord p) => Procedure p d -> Bool
mostPreciseIsSecure p = heckerOf (mostPreciseLabeling p)

mostPreciseLabeling :: (Ord p) => Procedure p d -> Procedure p p
mostPreciseLabeling pr@(Procedure { input, output, influences}) = pr {
      includes = includes
    }
  where includes p
          | p ∈ input  = S.fromList [p]
          | p ∈ output = S.fromList [i | i <- toList input, p ∈ influences i]
          | otherwise = S.fromList [] -- TODO: require some wellformedness for procedures






-- "weakings pr" enumerates all weakenings of pr, i.e. all procedures pr' such that
--      pr `isNaivelyStrongerThan` pr'
-- as defined below
weakenings :: (Ord d, Ord p, Enum d, Bounded d) => Procedure p d -> [Procedure p d]
weakenings pr@(Procedure { input, output, includes, influences}) =
   [ pr { includes = \p -> fromJust $ lookup p choice } | choice <- choices ]
  where choices = chooseOneEach $    [(i, [d | d <- toList $ powerset $ fromList allValues, d ⊆ includes i]) | i <- toList $  input]
                                  ++ [(o, [d | d <- toList $ powerset $ fromList allValues, d ⊇ includes o]) | o <- toList $ output]

        chooseOneEach :: [(a,[b])] -> [[(a,b)]]
        chooseOneEach choices = fmap (zip as) $ sequence bss
          where as  = fmap fst choices
                bss = fmap snd choices

-- given two different ifc requirements for the same procedure,
-- i.e.: given procedures pr, pr' such that
--   * input  pr == input  pr'
--   * output pr == output pr'
--   * influences pr == influences pr'
-- , the ifc requirement of pr is called "naively stronger" than that of pr' iff
--      pr `isNaivelyStrongerThan` pr'
-- as defined here.
isNaivelyStrongerThan ::  (Ord d) => Procedure p d ->  Procedure p d -> Bool
pr `isNaivelyStrongerThan` pr'  =
      and [ includes pr' p ⊆ includes pr p | p <- toList $ input pr]
  &&  and [ includes pr' p ⊇ includes pr p | p <- toList $ output pr]



-- Some QuickCheck predicates concerning weakenings
--
-- 1) weakenings do indeed weaken the ifc requirement
weakeningsAreWeaker :: (Enum d, Bounded d, Ord d, Ord p) => Procedure p d -> Bool
weakeningsAreWeaker pr = and [ pr `isNaivelyStrongerThan` pr' | pr' <- weakenings pr ]

-- 2) if pr fullfills its ifc requirement, then also all weakenings of pr do
weakeningsAreSafe :: (Enum d, Bounded d, Ord d, Ord p) => Procedure p d -> Property
weakeningsAreSafe pr = heckerOf pr ==>
  and [ heckerOf pr' |  pr' <- weakenings pr ]




isConsistentRelabelingFor :: forall d d' p. Ord d => (d' -> Set d) -> Procedure p d -> Procedure p d' -> Bool
isConsistentRelabelingFor g0 pr pr' =  pr `isNaivelyStrongerThan` pr'Relabeled
  where g :: Set d' -> Set d
        g = gFrom g0
        pr'Relabeled :: Procedure p d
        pr'Relabeled = pr' { includes = \p -> g (includes pr' p) }

intersections :: (Ord a, Enum a, Bounded a) => [Set a] -> Set a
intersections = P.foldl S.intersection (fromList allValues)

lowerAdjoint :: (Bounded d', Enum d', Ord d', Ord d) => (Set d' -> Set d) -> (Set d -> Set d')
lowerAdjoint g ds = intersections [ ds' | ds' <- toList $ powerset (fromList allValues), ds ⊆ g ds' ]

upperAdjoint :: (Bounded d, Enum d, Ord d', Ord d) => (Set d -> Set d') -> (Set d' -> Set d)
upperAdjoint f ds' = unions [ ds | ds <- toList $ powerset (fromList allValues), f ds ⊆ ds' ]

gFrom g0 ds' = unions [ g0 d' | d' <- toList ds']


hasFewerFlowsThan ::  (Ord p) => Procedure p d ->  Procedure p d' -> Bool
pr `hasFewerFlowsThan` pr'  =
      and [ influences pr p ⊆ influences pr' p | p <- toList $ input pr]


fewerFlowsIffSecure  :: forall d p. (Ord d, Ord p) => Procedure p d -> Bool
fewerFlowsIffSecure procedure@(Procedure { input, output, includes, influences}) =
       (secure procedure classifiedAsHecker latticeHecker)
   ==  (procedure `hasFewerFlowsThan` (γ procedure))
  where (classifiedAsHecker,  latticeHecker) = hecker procedure


γ :: (Ord d, Ord p) => Procedure p d -> Procedure p d
γ procedure@(Procedure { input, output, includes }) =
  procedure { influences = \p ->
                if (p ∈ input) then fromList [ p' | p' <- toList output, includes p ⊆ includes p' ]
                               else fromList []
            }

-- .. or equivalently:
γ' :: (Ord d, Ord p) => Procedure p d -> Procedure p d
γ' procedure@(Procedure { input, output, includes }) =
  procedure { influences = \p ->
                if (p ∈ input) then output ∖ fromList [ p' | p' <- toList output, ds <- toList $ includes p,
                                                                                  not $ ds ∈ includes p' ]
                               else fromList []
            }
γIsγ' :: (Show d, Show p, Enum p, Bounded p, Ord d, Ord p) => Procedure p d -> Bool
γIsγ' pr = (show $ γ pr) == (show $ γ' pr) -- TODO: dont use hacky string-comparison


γMostPreciseIsMostPrecuse :: (Ord d, Ord p) => Procedure p d -> Bool
γMostPreciseIsMostPrecuse pr = and [ influences pr p == influences pr' p | p <- toList $ input pr ]
  where pr' = γ (mostPreciseLabeling pr)

-- given two different ifc requirements for the same procedure,
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

-- .. which is simply the expansion of:    (γ pr) `hasFewerFlowsThan` (γ pr'):
isStrongerThanIsHasFewerFlowsThan ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Bool
isStrongerThanIsHasFewerFlowsThan (SpecificationPair pr pr') =
      (γ pr) `hasFewerFlowsThan` (γ pr')
  ↔     pr  `isStrongerThan`       pr'



-- .. and more applicable than `isStrongerThan`:
-- paranoids will want to check with:
--    quickCheckWith  stdArgs { maxDiscardRatio = 200 } (isStrongerThanIsBetterThanIsNaivelyStrongerThan :: SpecificationPair Parameter Datasets Datasets -> Property)
isStrongerThanIsBetterThanIsNaivelyStrongerThan ::  (Ord p, Ord d) => SpecificationPair p d d -> Property
isStrongerThanIsBetterThanIsNaivelyStrongerThan (SpecificationPair pr pr') =
       pr  `isNaivelyStrongerThan` pr'
  ==>  pr  `isStrongerThan`        pr'



-- Ultimately, we're interested in the following property.
-- paranoids will want to check with:
-- quickCheckWith  stdArgs { maxDiscardRatio = 400 } (isStrongerThanIsJustified :: SpecificationPair Parameter Datasets Datasets -> Property)
isStrongerThanIsJustified ::  (Ord p, Ord d, Ord d') => SpecificationPair p d d' -> Property
isStrongerThanIsJustified (SpecificationPair pr pr') =
       (
            (secure pr  classifiedAsHecker  latticeHecker )
        &&  (pr  `isStrongerThan`        pr')
       )
   ==>      (secure pr' classifiedAsHecker' latticeHecker')
  where (classifiedAsHecker ,  latticeHecker ) = hecker pr
        (classifiedAsHecker',  latticeHecker') = hecker pr'
