{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Noninterference.Export where

import Noninterference.Util
import Noninterference.Component

import Control.Monad.Random (getStdRandom, randomR)
import System.Process (runInteractiveCommand, system)

import Data.List (intersperse)
import Data.Set as S
import Data.Map as Map

toTikz :: (Show p, Ord p, Show d) =>  Component p -> Implementation p -> Specification p d -> String
toTikz = toTikzQuestionMark True False

toTikzQuestionMark :: (Show p, Ord p, Show d) => Bool -> Bool -> Component p -> Implementation p -> Specification p d -> String
toTikzQuestionMark showSpec questionMark (Component { input, output }) (Implementation { influences }) (Specification { includes })  =
    unlines $ [
      "\\begin{tikzpicture}[node distance=1cm, auto, framed, background rectangle/.style={ultra thick,draw=kit, fill=kit, fill opacity=0.15, rounded corners}  ]"
    ]
    ++
    [ "  \\node (input"  ++ (show i) ++ ") at ( 1, " ++ (show $ -i*3) ++ ") { " ++ (show p) ++ " };" | p <- S.toList input,  let i = input2Index ! p]
    ++
    [ "  \\node (output" ++ (show i) ++ ") at ( 5, " ++ (show $ -i*3) ++ ") { " ++ (show p) ++ " };" | p <- S.toList output, let i = output2Index ! p]
    ++
    [ "  \\draw[color=black,thick, -{>[scale=2.5, length=2, width=3]}] (input" ++ (show iIn) ++ ") --  (output" ++ (show iOut) ++ ");" | pIn <- S.toList input,             let iIn  = input2Index  ! pIn,
                                                                                   pOut <- S.toList (influences pIn), let iOut = output2Index ! pOut
    ]
    ++ ( if showSpec then 
    [ "  \\node[left=of input"   ++ (show i) ++ "] { " ++ (showSet $ includes p) ++ " };" | p <- S.toList input,  let i = input2Index ! p ]
    ++
    [ "  \\node[right=of output" ++ (show i) ++ "] { " ++ (showSet $ includes p) ++ " };" | p <- S.toList output, let i = output2Index ! p ]
    else []
    )
    ++ [
      "  \\filldraw[draw=kit, fill=kit, fill opacity=0.15, rounded corners]",
      "                                         (input1.north west) --",
      "                                         (output1.north east) --"
    ]
    ++ (
      if (maxOutputIndex >= maxInputIndex) then [
      "                                         (output" ++ (show maxOutputIndex) ++ ".south east) --",
      "                                         (input"  ++ (show maxInputIndex)  ++".south west |- output" ++ (show maxOutputIndex) ++ ".south west) --"
      ] else [
      "                                         (output" ++ (show maxOutputIndex) ++".south east |- input" ++ (show maxInputIndex) ++ ".south east) --",
      "                                         (input"  ++ (show maxInputIndex)  ++ ".south west) --"
      ]
    )
    ++
    [
      "                                         cycle;"
    ]
    ++ (
      if (questionMark) then [
      "  \\node at (3, " ++ (show $ -(3*((max maxInputIndex maxOutputIndex) + 1) `div` 2)) ++ ") [fontscale=10] { ... };"
      ] else []
    )
    ++
    [
      "\\end{tikzpicture}"
    ]

  where input2Index   = Map.fromList [ (p,i) | (p,i) <- zip (S.toList input)  [1..]]
        output2Index  = Map.fromList [ (p,i) | (p,i) <- zip (S.toList output) [1..]]
        maxInputIndex  = Map.fold (max) 0 input2Index
        maxOutputIndex = Map.fold (max) 0 output2Index
        showSet s = "\\{ " ++ (concat $ intersperse ", " [ show x | x <- S.toList s ]) ++ " \\}"



toTikzNamed :: (Show p, Ord p, Show d) => String -> Bool -> Bool -> Component p -> Implementation p -> Specification p d -> String
toTikzNamed name showSpec questionmark co impl sp = unlines [
    "\\newcommand{\\" ++ name ++ "}{%"
  ]
  ++
  (toTikzQuestionMark showSpec questionmark co impl sp)
  ++ unlines [
    "}"
  ]

toTikzComplete  :: (Show p, Ord p, Show d) => Component p -> Implementation p -> Specification p d -> String
toTikzComplete co impl sp = unlines [
    "\\documentclass[a4paper,landscape]{article}",
    "\\usepackage{tikz}",
    "\\usetikzlibrary{arrows,positioning}",
    "\\usetikzlibrary{backgrounds}",
    "\\usetikzlibrary{arrows.meta}",
    "\\usepackage{relsize}", 
    "\\tikzset{fontscale/.style = {font=\\relsize{#1}}}",
    "\\definecolor{kit}{RGB}{0,150,130}",
    "\\begin{document}"
  ]
  ++
  (toTikz co impl sp)
  ++ unlines [
    "\\end{document}"
  ]


showComponent co impl sp = do
  let tex = toTikzComplete co impl sp
  randomInt <- getStdRandom (randomR (1,65536)) :: IO Int
  let file = "tmpfile" ++ (show randomInt)
  writeFile (file ++ ".tex")  tex
  system                $ "pdflatex -interaction=batchmode " ++ (file ++ ".tex")
  runInteractiveCommand $ "evince " ++ (file ++ ".pdf")

