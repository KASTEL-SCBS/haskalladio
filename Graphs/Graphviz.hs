{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module      : Zora.TreeGraphing
-- Copyright   : (c) Brett Wines 2014
--
-- License	   : BSD-style
--
-- Maintainer  : bgwines@cs.stanford.edu
-- Stability   : experimental
-- Portability : portable
--
-- Modifications : martin.hecker@kit.edu


module Graphs.Graphviz
where
  
import System.Directory (removeFile, getDirectoryContents)
import Control.Exception
import System.IO.Error hiding (catch)

import System.IO.Unsafe

import Data.Maybe
import Data.Tuple

import Data.Monoid

import qualified Data.Map as M
import qualified Data.List as L hiding (zip, map, length, take, drop, takeWhile, last, filter, concatMap)
import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as ByteString

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete hiding (value, Label)
import Data.Word

type Label = Int


asgraph ::
           (g -> Bool) ->
           (g -> String) ->
           (g -> [g]) ->
           g -> ([LNode Ly.Text], [LEdge Ly.Text])
asgraph is_empty value get_children g = (nodes, edges)
		where
			nodes :: [LNode Ly.Text]
			nodes = zip [0..] $ map show' nodes_in_g

			show' = Ly.pack . value

			nodes_in_g = zoldMap (\a -> [a]) g

			edges :: [LEdge Ly.Text]
			edges = concatMap edgeify nodes_in_g

			edgeify node =
				catMaybes . map maybe_edge . get_children $ node
				where 
					maybe_edge child = if is_empty child
						then Nothing
						else Just
							( m M.! (show' node)
							, m M.! (show' child)
							, Ly.empty )

					m :: M.Map Ly.Text Label
					m = M.fromList $ map swap nodes
                        zoldMap f node =
                          if is_empty node
                            then mempty
                            else (f node) `mappend` (mconcat . map (zoldMap f) . get_children $ node)


asgraphs ::
           (g -> Bool) ->
           (g -> String) ->
           (g -> [g]) ->
           [g] -> ([LNode Ly.Text], [LEdge Ly.Text])
asgraphs is_empty value get_children g = (nodes, edges)
		where
			nodes :: [LNode Ly.Text]
			nodes = zip [0..] $ [show' node | node <- nodes_in_g]

			show' = Ly.pack . value

                        nodes_in tree = zoldMap (\a -> [a]) tree
			nodes_in_g = [node | tree <- g, node <- nodes_in tree]

			edges :: [LEdge Ly.Text]
			edges = concatMap edgeify nodes_in_g

			edgeify node =
				catMaybes . map maybe_edge . get_children $ node
				where 
					maybe_edge child = if is_empty child
						then Nothing
						else Just
							( m M.! (show' node)
							, m M.! (show' child)
							, Ly.empty )

					m :: M.Map Ly.Text Label
					m = M.fromList $ map swap nodes
                        zoldMap f node =
                          if is_empty node
                            then mempty
                            else (f node) `mappend` (mconcat . map (zoldMap f) . get_children $ node)




as_dotfile
		= Ly.unpack
		. printDotGraph
		. graphToDot params
		. mkGraph'
		where
			mkGraph' :: ([LNode Ly.Text], [LEdge Ly.Text]) -> (Gr Ly.Text Ly.Text)
			mkGraph' (v, e) = mkGraph v e

			params :: GraphvizParams n Ly.Text Ly.Text () Ly.Text
			params = nonClusteredParams { globalAttributes = ga
										, fmtNode = fn
										, fmtEdge = fe }
				where
					fn (_,l) = [textLabel l]
					fe (_,_,l) = [textLabel l]

					ga = [ GraphAttrs [ RankDir	 FromTop
									  , BgColor	 [toWColor White] ]
						 , NodeAttrs	[ shape	 BoxShape
										-- , FillColor (some_color 4)
										-- , style	 filled
										, Width	 0.1
										, Height	0.1 ] ]
