{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Noninterference.Export where

import Noninterference.Util
import Noninterference.Procedure

import Data.Set as S
import Data.Map as Map

toTikz :: (Show p, Ord p, Show d) => Procedure p d -> String
toTikz (Procedure { input, output, includes, influences}) =
    unlines $ [
      "\\begin{tikzpicture}[node distance=1cm, auto]"
    ]
    ++
    [ "  \\node (input"  ++ (show i) ++ ") at ( 1, " ++ (show $ i*3) ++ ") { " ++ (show p) ++ " };" | p <- S.toList input,  let i = input2Index ! p]
    ++
    [ "  \\node (output" ++ (show i) ++ ") at ( 5, " ++ (show $ i*3) ++ ") { " ++ (show p) ++ " };" | p <- S.toList output, let i = output2Index ! p]
    ++
    [ "  \\draw (input" ++ (show iIn) ++ ") --  (output" ++ (show iOut) ++ ");" | pIn <- S.toList input,             let iIn  = input2Index  ! pIn,
                                                                                   pOut <- S.toList (influences pIn), let iOut = output2Index ! pOut
    ]
    ++
    [ "  \\node[left=of input"   ++ (show i) ++ "] { " ++ (show $ includes p) ++ " };" | p <- S.toList input,  let i = input2Index ! p ]
    ++
    [ "  \\node[right=of output" ++ (show i) ++ "] { " ++ (show $ includes p) ++ " };" | p <- S.toList output, let i = output2Index ! p ]
    ++ [
      "\\end{tikzpicture}"
    ]

  where input2Index   = Map.fromList [ (p,i) | (p,i) <- zip (S.toList input)  [1..]]
        output2Index  = Map.fromList [ (p,i) | (p,i) <- zip (S.toList output) [1..]]




toTikzComplete  :: (Show p, Ord p, Show d) => Procedure p d -> String
toTikzComplete pr = unlines [
    "\\documentclass[a4paper,landscape]{article}",
    "\\usepackage{tikz}",
    "\\usetikzlibrary{arrows,positioning}",
    "\\begin{document}"
  ]
  ++
  (toTikz pr)
  ++ unlines [
    "\\end{document}"
  ]
