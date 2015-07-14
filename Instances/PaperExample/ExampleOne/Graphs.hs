module Instances.SmartHome.ExampleOne.Graphs where

import Palladio
import Instances.SmartHome.ExampleOne.Palladio
import Security
import Reasons

import Control.Monad.Trans.Writer.Lazy


import AbstractAnalysis

import Instances.SmartHome.ExampleOne.Security
import Instances.SmartHome.ExampleOne.TamperableLinkModel

import Misc
import Data.Set.Monad as Set

import Graphs.Graphviz




data Node m = Root String [Reason m] | R (Reason m) deriving Show



isempty :: (Reason m) -> Bool
isempty _ = False

value :: (Reason m) -> String
value (Axiom1 r x)   = "Axiom1: " ++ show r ++ "(" ++ show x ++ ")"
value (Axiom2 r x y) = "Axiom2: " ++ show r ++ "(" ++ show x ++ "," ++ show y ++ ")"
value (MapsTo f x y) = show f ++ "(" ++ show x ++  ") == " ++ show y
value (Inferred2 r x y rs) = "Inferred2: " ++ show r ++ "(" ++ show x ++ "," ++ show y ++ ")"
value (Not r)              = "Not(" ++ value r ++ ")"

children :: (Reason m) -> [Reason m]
children (Inferred2 r x y rs) = rs
children _ = []


isemptyN (Root s rs) = False
isemptyN (R r) = False


valueN (Root s rs) = s
valueN (R r) = value r

childrenN (Root s rs) = fmap R rs
childrenN (R r) = fmap R (children r)


trees = fmap (Root "insecure(Burglar)") $ fmap snd $ toList $ runWriterT $ isInSecureWithRespectTo Burglar

graphs = asgraphs isemptyN valueN childrenN trees

tograph = asgraph isemptyN valueN childrenN


-- writeFile "burglar.dot" $ as_dotfile graphs

