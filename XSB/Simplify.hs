module Simplify where

import Prettyprint hiding (main)

import Text.Read

import Data.List (nub)
import Data.Tree (drawTree)

import Control.Monad
import System.Environment(getArgs)



removeValues :: Proof -> Proof
removeValues proof = fmap f proof
  where f (Assertion t)   = Assertion (fmap g t)
        f (Not a)         = Not (f a)
        f (NotEq ts1 ts2) = NotEq (fmap (fmap g) ts1) (fmap (fmap g) ts2)

        g atom = case (readMaybe atom :: Maybe Integer) of
          Just x -> "_"
          _      -> atom



prettyPrintWith :: (Proof -> Proof) -> String -> IO ()
prettyPrintWith f file =
  forM_ (fmap drawTree $  fmap (fmap show) (nub $ fmap f $ fromFile file)) putStrLn


main = do
       [file] <- getArgs
       prettyPrintWith removeValues file
