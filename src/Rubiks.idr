module Rubiks

import Data.DPair
import Data.Graph
import Data.Graph.Cayley
import Data.Group
import Data.Group.Cyclic
import Data.Group.Permutation
import Data.Group.Permutation.Vect
import Data.List.Lazy
import Data.Vect
import Generics.Derive

%default total
%language ElabReflection

record CubeCornerOrientation repr where
  constructor MkCubeCornerOrientation
  c0, c1, c2, c3, c4, c5, c6 : repr

record CubeCorners (perm : Nat -> Type) (cyc : Nat -> Type) where
  constructor MkCubeCorners
  permutation : perm 7
  -- orientation : CubeCornerOrientation (cyc 3)

-- %runElab derive "CubeCorners" [Generic, Meta, Eq, Ord, Show]

record Cube where
  constructor MkCube
  permutation : PVect 8

%runElab derive "CubeCorners" [Generic, Eq, Ord, Semigroup, Monoid, Group]

target : PVect 8
target = MkPVect [0, 1, 2, 3, 4, 5, 6, 7]

scramble : PVect 8
scramble = MkPVect [2, 3, 0, 1, 4, 5, 6, 7]

gens : List (PVect 8)
gens =
  [ MkPVect [1, 2, 3, 0, 4, 5, 6, 7] -- U
  , MkPVect [1, 7, 2, 3, 0, 5, 6, 4] -- R
  ]

solution : Maybe (List (PVect 8))
solution = head' $ dfs target scramble 4
