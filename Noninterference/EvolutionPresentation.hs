import Unicode
import Security
import Noninterference
import Noninterference.Util
import Noninterference.Procedure

import Data.Set as S
import Data.Set.Unicode

import qualified Data.Map as M


data AbstractDatasets = D0
                      | D1
                      | D2
                      deriving (Show, Eq, Ord, Enum, Bounded)


data SystemDatasets = X
                    | Y
                    | Z
                    deriving (Show, Eq, Ord, Enum, Bounded)

data Parameter = R
               | A
               | B
               deriving (Show, Eq, Ord, Enum, Bounded)

foo :: Procedure Parameter AbstractDatasets
foo = Procedure {
      input  = S.fromList [A,B],
      output = S.fromList [R],
      includes = includes,
      influences = influences
    }
  where
    includes R = S.fromList $ [D0]
    includes A = S.fromList $ [D1,D2]
    includes B = S.fromList $ []

    influences R = S.fromList []
    influences A = S.fromList []
    influences B = S.fromList []


replacement1c D0 = S.fromList [X]
replacement1c D1 = S.fromList [Y]
replacement1c D2 = S.fromList [Y]
foo1c :: Procedure Parameter SystemDatasets
foo1c = foo {
      includes = includes
    }
  where
    includes R = S.fromList $ [X]
    includes A = S.fromList $ [Y]
    includes B = S.fromList $ []


replacement1d D0 = S.fromList [X]
replacement1d D1 = S.fromList [Y]
replacement1d D2 = S.fromList [Z]
foo1d :: Procedure Parameter SystemDatasets
foo1d = foo {
      includes = includes
    }
  where
    includes R = S.fromList $ [X]
    includes A = S.fromList $ [Y,Z]
    includes B = S.fromList $ []


replacement2c = replacement1d
foo2c :: Procedure Parameter SystemDatasets
foo2c = foo1d

replacement2d = replacement1c
foo2d :: Procedure Parameter SystemDatasets
foo2d = foo1c
