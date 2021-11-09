module Rubiks.Cube

import Data.Enum
import Data.Group
import Data.Group.Generics
import Data.Group.Permutation.Vect
import Generics.Derive

%default total
%language ElabReflection

public export
record Cube (n : Nat) where
  constructor MkCube
  permutation : PVect 8

%runElab derive "Cube" [Generic, Eq, Semigroup, Monoid, Group]

public export
Generate (PVect 8) g => Generate (Cube n) g where
  (MkCube p) <++> g = MkCube $ p <++> g
