-- requires aeson and aeson-pretty to be installed

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE CPP #-}
-- #define USE_GENERIC_TOJSON_INSTANCE

module PrettyprintToJSON where 

import Prettyprint hiding (main)

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson.Encode.Pretty

import Prelude
import Data.Char(chr)
import Data.Maybe (fromJust)
import Data.List (find, intercalate, nub, intersperse)
import Data.Tree
import qualified Data.Map (lookup, fromList) 
import Text.Read (readMaybe)

import System.IO
import System.IO.Unsafe
import System.Environment(getArgs)

import Control.Monad
import Control.Applicative ((<*),(*>))

import qualified Data.Vector as Vector

{- Test with something like:
  let { p = Node (Not $ NotEq [Node "a" []] [Node "b" [] ])  []  :: Proof } in LBS.putStrLn  $ encode p
-}
#ifdef USE_GENERIC_TOJSON_INSTANCE
instance ToJSON Assertion where
    toEncoding = genericToEncoding defaultOptions
#else
instance ToJSON Assertion where
  toJSON (Assertion t )   = termToJSON t
  toJSON (AbbreviatedProof t n) =
                            object ["abbreviated" .= termToJSON t, "n" .= n ]
  toJSON (Not a )         = object ["not"         .= toJSON a ]
  toJSON (NotEq ts1 ts2 ) = object ["notEq"       .= [ toJSON ts1, toJSON ts2] ]
  toJSON (World world)    = object ["world"       .= termToJSON world ]


{- this overlaps with the Data.Tree instance -}
instance ToJSON Proof where
  toJSON = proofToJSON

{- this overlaps with the Data.Tree instance -}
instance ToJSON Term where
  toJSON = termToJSON

proofToJSON :: Proof -> Value
proofToJSON (Node a []) = toJSON a
proofToJSON (Node a as) = object [ "conclusion" .= toJSON a, "premises" .= toJSON (fmap toJSON as) ]

termToJSON :: Term -> Value
termToJSON (Node a []) = toJSON a
termToJSON (Node a ts) = toJSON [ toJSON a, toJSON (fmap termToJSON ts) ]
#endif



prettyPrintToJSON :: String -> String -> IO ()
prettyPrintToJSON file descsFile =
  forM_ worlds (\(world,proofs) -> do
      LBS.putStrLn $ encodePretty $ Vector.fromList $ map (\p -> proofToJSON $ insertDescriptions descs $ interestingOnly $ termSubst flattenWorld p) proofs
   )
 where descs = descriptionFromFile descsFile
       worlds = fromFile file
       flattenWorld w@(Node "world" _) = Node (showWorld Subs w) []
       flattenWorld x                  = x

main = do
       [file, descriptionFile] <- getArgs
       prettyPrintToJSON file descriptionFile

