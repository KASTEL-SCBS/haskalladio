{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

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

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

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

showMapFun f = show $ fromList $ [(x, f x) | x <- allValues]


instance Show (Procedure Parameter Datasets) where
  show (Procedure { input, output, includes, influences}) =
    "Procedure { input = " ++ (show input) ++ ", output = " ++ (show output) ++ ", includes = " ++ (showMapFun includes) ++ ", influences = " ++ (showMapFun influences) ++ " }"

instance Arbitrary (Procedure Parameter Datasets) where
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


weakenings :: (Ord d, Ord p, Enum d, Bounded d) => Procedure p d -> [Procedure p d]
weakenings pr@(Procedure { input, output, includes, influences}) =
   [ pr { includes = \p -> fromJust $ lookup p choice } | choice <- choices ]
  where choices = chooseOneEach $    [(i, [d | d <- toList $ powerset $ fromList allValues, d ⊆ includes i]) | i <- toList $  input]
                                  ++ [(o, [d | d <- toList $ powerset $ fromList allValues, d ⊇ includes o]) | o <- toList $ output]

        chooseOneEach :: [(a,[b])] -> [[(a,b)]]
        chooseOneEach choices = fmap (zip as) $ sequence bss
          where as  = fmap fst choices
                bss = fmap snd choices

isWeakerThan ::  (Ord d) => Procedure p d ->  Procedure p d -> Bool
pr' `isWeakerThan` pr  =
      and [ includes pr' p ⊆ includes pr p | p <- toList $ input pr]
  &&  and [ includes pr' p ⊇ includes pr p | p <- toList $ output pr]

weakeningsAreWeaker :: (Enum d, Bounded d, Ord d, Ord p) => Procedure p d -> Bool
weakeningsAreWeaker pr = and [ pr' `isWeakerThan` pr | pr' <- weakenings pr ]

weakeningsAreSafe :: (Enum d, Bounded d, Ord d, Ord p) => Procedure p d -> Property 
weakeningsAreSafe pr = heckerOf pr ==>
  and [ heckerOf pr' |  pr' <- weakenings pr ]

