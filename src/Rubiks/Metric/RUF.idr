module Rubiks.Metric.RUF

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

||| The semantics of RUF on edge permutation.
||| UF = 0, UR = 1, UB = 2, UL = 3,
||| FR = 4, BR = 5, BL = 6, FL = 7,
||| DF = 8, DL = 9, DB = 10, DR = 11
public export
Generate (PVect 12) RUF where
  p <++> R  = p <+> (MkPVect . asVect) [[1, 5, 11, 4]]
  p <++> U  = p <+> (MkPVect . asVect) [[0, 1, 2, 3]]
  p <++> F  = p <+> (MkPVect . asVect) [[0, 4, 8, 7]]
  p <++> R2 = p <+> (MkPVect . asVect) [[1, 11], [4, 5]]
  p <++> U2 = p <+> (MkPVect . asVect) [[0, 2], [1, 3]]
  p <++> F2 = p <+> (MkPVect . asVect) [[0, 8], [4, 7]]
  p <++> R' = p <+> (MkPVect . asVect) [[1, 4, 11, 5]]
  p <++> U' = p <+> (MkPVect . asVect) [[0, 3, 2, 1]]
  p <++> F' = p <+> (MkPVect . asVect) [[0, 7, 8, 4]]

||| The semantics of RUF on edge orientation.
||| Oriented = 0, Flipped = 1
||| Order is as given in the permutation implementation
public export
Generate (Vect 12 (C 2)) RUF where
  c <++> R  = c <+> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  c <++> U  = c <+> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  c <++> F  = c <+> [1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0]
  c <++> R2 = c <+> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  c <++> U2 = c <+> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  c <++> F2 = c <+> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  c <++> R' = c <+> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  c <++> U' = c <+> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  c <++> F' = c <+> [1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0]
