{-# LANGUAGE NamedFieldPuns #-}
import Unicode
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


main = forM_ [(example1, example1Impl, example1Spec),
              (example2, example2Impl, example2Spec),
              (example3, example3Impl, example3Spec),
              (example4, example4Impl, example4Spec),
              (example5, example5Impl, example5Spec),
              (example6, example6Impl, example6Spec)
            ]
        (\(pr,impl,sp) -> putStrLn $ toLatex pr impl sp)



toLatex :: Component Parameter -> Implementation Parameter -> Specification Parameter Datasets -> String
toLatex pr@(Component { input, output }) impl@(Implementation { influences }) sp@(Specification { includes })
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
     "\\securewrt{i }{" ++ (if (NI.secure  NI.key  pr impl sp) then "\\CheckedBox" else "\\Square") ++ "}",
     "\\securewrt{ii}{" ++ (if (NIG.secure NIG.key pr impl sp) then "\\CheckedBox" else "\\Square") ++ "}",
     "",
     "\\end{frame}",
     ""
    ]

  where toVar = (fmap toLower).show
        toDs  = (fmap toLower).show
        parameters = S.toList $ input ∪ output



example1 :: Component Parameter
example1 = Component {
      input  = S.fromList [A],
      output = S.fromList [Return]
    }

example1Impl :: Implementation Parameter
example1Impl = Implementation {
      influences = influences
    }
  where
    influences A = S.fromList [Return]

example1Spec :: Specification Parameter Datasets
example1Spec = Specification {
      includes = includes,
      datasets = S.fromList allValues
    }
  where
    includes A      = S.fromList $ [D1]
    includes Return = S.fromList $ [D0]


example2 = example1

example2Impl = Implementation {
      influences = influences
    }
  where
    influences A = S.fromList [Return]

example2Spec = Specification {
      includes = includes,
      datasets = S.fromList allValues
    }
  where
    includes A      = S.fromList $ [D1,D2]
    includes Return = S.fromList $ [D1]



example3 = example1

example3Impl :: Implementation Parameter
example3Impl = Implementation {
      influences = influences
    }
  where
    influences A = S.fromList [Return]

example3Spec :: Specification Parameter Datasets
example3Spec = Specification {
      includes = includes,
      datasets = S.fromList allValues
    }
  where
    includes A      = S.fromList $ []
    includes Return = S.fromList $ [D1]


example4 = example1

example4Impl :: Implementation Parameter
example4Impl = Implementation {
      influences = influences
    }
  where
    influences A = S.fromList [Return]

example4Spec :: Specification Parameter Datasets
example4Spec = Specification {
      includes = includes,
      datasets = S.fromList allValues
    }
  where
    includes A      = S.fromList $ [D1]
    includes Return = S.fromList $ []


example5 :: Component Parameter
example5 = Component {
      input  = S.fromList [A,B],
      output = S.fromList [Return]
    }

example5Impl :: Implementation Parameter
example5Impl = Implementation {
      influences = influences
    }
  where
    influences A = S.fromList [Return]
    influences B = S.fromList [Return]
    influences _ = (∅)

example5Spec :: Specification Parameter Datasets
example5Spec = Specification {
      includes = includes,
      datasets = S.fromList allValues
    }
  where
    includes A      = S.fromList $ [D1]
    includes B      = S.fromList $ [D2]
    includes Return = S.fromList $ [D1,D2]


example6 :: Component Parameter
example6 = example5

example6Impl :: Implementation Parameter
example6Impl = Implementation {
      influences = influences
    }
  where
    influences A = S.fromList [Return]
    influences B = S.fromList [Return]
    influences _ = (∅)

example6Spec :: Specification Parameter Datasets
example6Spec = Specification {
      includes = includes,
      datasets = S.fromList allValues
    }
  where
    includes A      = S.fromList $ [D1,D3]
    includes B      = S.fromList $ [D2,D3]
    includes Return = S.fromList $ [D3]


example7 :: Component Parameter
example7 = Component {
      input  = S.fromList [A,B],
      output = S.fromList [RA, RB]
    }

example7Impl :: Implementation Parameter
example7Impl = Implementation {
      influences = influences
    }
  where
    influences A = S.fromList [Return]
    influences B = S.fromList [Return]
    influences _ = (∅)

example7Spec :: Specification Parameter Datasets
example7Spec = Specification {
      includes = includes,
      datasets = S.fromList allValues
    }
  where
    includes A      = S.fromList $ [D1,D3]
    includes B      = S.fromList $ [D2,D3]
    includes Return = S.fromList $ [D3]



foo :: Component Parameter
foo = Component {
      input  = S.fromList [A,B],
      output = S.fromList [RBoth,RA,RB]
    }

fooImpl :: Implementation Parameter
fooImpl = Implementation {
      influences = influences
    }
  where
    influences RBoth = S.fromList []
    influences RA    = S.fromList []
    influences RB    = S.fromList []
    influences A = S.fromList [RBoth,RA]
    influences B = S.fromList [RBoth,RB]


fooSpec :: Specification Parameter Datasets
fooSpec = Specification {
      includes = includes,
      datasets = S.fromList allValues
    }
  where
    includes RBoth = S.fromList $ [D1,D2]
    includes RA    = S.fromList $ [D1]
    includes RB    = S.fromList $ [D2]
    includes A = S.fromList $ [D1]
    includes B = S.fromList $ [D2]
