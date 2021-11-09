module Rubiks.Cube

import Data.Group
import Data.Group.Generics
import Generics.Derive
import public Rubiks.Piece

%default total
%language ElabReflection

public export
record Cube (n : Nat) where
  constructor MkCube
  corners : Piece 8 3
  edges : Piece 12 2

%runElab derive "Cube" [Generic, Meta, Eq, Semigroup, Monoid, Group]

public export
Generate (Piece 8 3) g => Generate (Piece 12 2) g => Generate (Cube n) g where
  c <++> g = MkCube (c.corners <++> g) (c.edges <++> g)
