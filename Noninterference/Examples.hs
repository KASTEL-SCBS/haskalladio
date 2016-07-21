{-# LANGUAGE NamedFieldPuns #-}
import Unicode
import Security
import qualified Noninterference        as NI
import qualified NoninterferenceGreiner as NIG
import Noninterference.Util
import Noninterference.Procedure
import Noninterference.Comparison

import Data.Set as S
import Data.Set.Unicode
import Data.List
import Data.Char

import Control.Monad(forM_)

import qualified Data.Map as M


data Datasets = D0
              | D1
              | D2
              | D3
              deriving (Show, Eq, Ord, Enum, Bounded)


data Parameter = RBoth
               | RA
               | RB
               | A
               | B
               | Return
               deriving (Show, Eq, Ord, Enum, Bounded)


main = forM_ [example1,
              example2,
              example3,
              example4,
              example5,
              example6
            ]
        (\e -> putStrLn $ toLatex e)



toLatex :: Procedure Parameter Datasets -> String
toLatex pr@(Procedure { input, output, includes, influences})
  | output /= S.fromList [Return] = error "rofl"
  | otherwise          =
    unlines [
     "",
     "\\begin{frame}[fragile,t]{Quiz}",
     ""
    ] ++ unlines [
       "\\includesstereo{" ++ (toDs ds) ++ "}{" ++
        intercalate "," (fmap toVar $ [ x | x <- S.toList $ input ∪ output, ds ∈ includes x ])
        ++ "} \\\\"
      | ds <- S.toList $ S.unions [ includes x | x <- parameters]
    ] ++ unlines [
     "",
     "\\vfill",
     "",
     "\\begin{lstlisting}",
     "int foo("  ++ intercalate "," (fmap (("int " ++).toVar) $ [ x | x <- S.toList input]                       ) ++ ") {",
     "  return " ++ intercalate "+" (fmap              toVar  $ [ x | x <- S.toList input, Return ∈ influences x]) ++ ";",
     "}",
     "\\end{lstlisting}",
     ""
    ] ++ unlines [
     "\\sicherunsicher",
     "",
     "\\securewrt{i }{" ++ (if (NI.secure  NI.key  pr) then "\\CheckedBox" else "\\Square") ++ "}",
     "\\securewrt{ii}{" ++ (if (NIG.secure NIG.key pr) then "\\CheckedBox" else "\\Square") ++ "}",
     "",
     "\\end{frame}",
     ""
    ]

  where toVar = (fmap toLower).show
        toDs  = (fmap toLower).show
        parameters = S.toList $ input ∪ output

example1 :: Procedure Parameter Datasets
example1 = Procedure {
      input  = S.fromList [A],
      output = S.fromList [Return],
      includes = includes,
      influences = influences
    }
  where
    includes A      = S.fromList $ [D1]
    includes Return = S.fromList $ [D0]

    influences A = S.fromList [Return]


example2 :: Procedure Parameter Datasets
example2 = Procedure {
      input  = S.fromList [A],
      output = S.fromList [Return],
      includes = includes,
      influences = influences
    }
  where
    includes A      = S.fromList $ [D1,D2]
    includes Return = S.fromList $ [D1]

    influences A = S.fromList [Return]



example3 :: Procedure Parameter Datasets
example3 = Procedure {
      input  = S.fromList [A],
      output = S.fromList [Return],
      includes = includes,
      influences = influences
    }
  where
    includes A      = S.fromList $ []
    includes Return = S.fromList $ [D1]

    influences A = S.fromList [Return]


example4 :: Procedure Parameter Datasets
example4 = Procedure {
      input  = S.fromList [A],
      output = S.fromList [Return],
      includes = includes,
      influences = influences
    }
  where
    includes A      = S.fromList $ [D1]
    includes Return = S.fromList $ []

    influences A = S.fromList [Return]



example5 :: Procedure Parameter Datasets
example5 = Procedure {
      input  = S.fromList [A,B],
      output = S.fromList [Return],
      includes = includes,
      influences = influences
    }
  where
    includes A      = S.fromList $ [D1]
    includes B      = S.fromList $ [D2]
    includes Return = S.fromList $ [D1,D2]

    influences A = S.fromList [Return]
    influences B = S.fromList [Return]
    influences _ = (∅)


example6 :: Procedure Parameter Datasets
example6 = Procedure {
      input  = S.fromList [A,B],
      output = S.fromList [Return],
      includes = includes,
      influences = influences
    }
  where
    includes A      = S.fromList $ [D1,D3]
    includes B      = S.fromList $ [D2,D3]
    includes Return = S.fromList $ [D3]

    influences A = S.fromList [Return]
    influences B = S.fromList [Return]
    influences _ = (∅)



example7 :: Procedure Parameter Datasets
example7 = Procedure {
      input  = S.fromList [A,B],
      output = S.fromList [RA, RB],
      includes = includes,
      influences = influences
    }
  where
    includes A      = S.fromList $ [D1,D3]
    includes B      = S.fromList $ [D2,D3]
    includes Return = S.fromList $ [D3]

    influences A = S.fromList [Return]
    influences B = S.fromList [Return]
    influences _ = (∅)


foo :: Procedure Parameter Datasets
foo = Procedure {
      input  = S.fromList [A,B],
      output = S.fromList [RBoth,RA,RB],
      includes = includes,
      influences = influences
    }
  where
    includes RBoth = S.fromList $ [D1,D2]
    includes RA    = S.fromList $ [D1]
    includes RB    = S.fromList $ [D2]
    includes A = S.fromList $ [D1]
    includes B = S.fromList $ [D2]

    influences RBoth = S.fromList []
    influences RA    = S.fromList []
    influences RB    = S.fromList []
    influences A = S.fromList [RBoth,RA]
    influences B = S.fromList [RBoth,RB]
