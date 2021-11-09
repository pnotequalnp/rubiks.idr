module Rubiks.Metric.RUF

import Data.Enum
import Data.Group.Permutation
import Data.Group.Cyclic
import Data.Group.Permutation.Vect
import Generics.Derive

%default total
%language ElabReflection

||| Abstract representation of a move group with three adjacent faces.
public export
data RUF = R  | U  | F
         | R2 | U2 | F2
         | R' | U' | F'

%runElab derive "RUF" [Generic, Meta, Eq, Show]

public export
Enum RUF where
  enum = [R, U, F, R2, U2, F2, R', U', F']

||| The semantics of RUF on corner permutation.
||| UFR = 0, UBR = 1, UBL = 2, UFL = 3,
||| DFR = 4, DFL = 5, DBL = 6, DBR = 7
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

||| The semantics of RUF on corner orientation.
||| Oriented = 0, CW = 1, CCW = 2
||| Order is as given in the permutation implementation
public export
Generate (Vect 8 (C 3)) RUF where
  c <++> R  = c <+> [1, 2, 0, 0, 2, 0, 0, 1]
  c <++> U  = c <+> [0, 0, 0, 0, 0, 0, 0, 0]
  c <++> F  = c <+> [2, 0, 0, 1, 1, 2, 0, 0]
  c <++> R2 = c <+> [0, 0, 0, 0, 0, 0, 0, 0]
  c <++> U2 = c <+> [0, 0, 0, 0, 0, 0, 0, 0]
  c <++> F2 = c <+> [0, 0, 0, 0, 0, 0, 0, 0]
  c <++> R' = c <+> [1, 2, 0, 0, 2, 0, 0, 1]
  c <++> U' = c <+> [0, 0, 0, 0, 0, 0, 0, 0]
  c <++> F' = c <+> [2, 0, 0, 1, 1, 2, 0, 0]
