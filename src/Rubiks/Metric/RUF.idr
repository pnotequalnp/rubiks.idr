module Rubiks.Metric.RUF

import Data.Enum
import Data.Group.Permutation
import Data.Group.Permutation.Vect
import Generics.Derive

%default total
%language ElabReflection

public export
data RUF = R  | U  | F
         | R2 | U2 | F2
         | R' | U' | F'

%runElab derive "RUF" [Generic, Meta, Eq, Show]

public export
Enum RUF where
  enum = [R, U, F, R2, U2, F2, R', U', F']

public export
Generate (PVect 8) RUF where
  p <++> R  = p <+> (MkPVect . asVect) [[0, 1, 7, 4]]
  p <++> U  = p <+> (MkPVect . asVect) [[0, 1, 2, 3]]
  p <++> F  = p <+> (MkPVect . asVect) [[0, 4, 5, 3]]
  p <++> R2 = p <+> (MkPVect . asVect) [[0, 7], [1, 4]]
  p <++> U2 = p <+> (MkPVect . asVect) [[0, 2], [1, 3]]
  p <++> F2 = p <+> (MkPVect . asVect) [[0, 5], [3, 4]]
  p <++> R' = p <+> (MkPVect . asVect) [[0, 4, 7, 1]]
  p <++> U' = p <+> (MkPVect . asVect) [[0, 3, 2, 1]]
  p <++> F' = p <+> (MkPVect . asVect) [[0, 3, 5, 4]]
