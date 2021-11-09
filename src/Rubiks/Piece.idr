module Rubiks.Piece

import Data.Group
import Data.Group.Cyclic
import Data.Group.Permutation.Vect
import Data.Vect
import Generics.Derive

%default total
%language ElabReflection

public export
record Piece (n : Nat) (o : Nat) where
  constructor MkPiece
  permutation : PVect n
  orientation : Vect n (C o)

%runElab derive "Piece" [Generic, Meta]

public export
Show (Piece n (S o)) where
  show p = show p.permutation <+> "\n" <+> show p.orientation

public export
Eq (Piece n o) where
  MkPiece p1 o1 == MkPiece p2 o2 = p1 == p2 && o1 == o2

public export
{n : Nat} -> {o : Nat} -> Semigroup (Piece n (S o)) where
  x <+> y = MkPiece (x.permutation <+> y.permutation) $
    permute x.permutation (x.orientation <+> y.orientation)

public export
{n : Nat} -> {o : Nat} -> Monoid (Piece n (S o)) where
  neutral = MkPiece neutral neutral

public export
{n : Nat} -> {o : Nat} -> Group (Piece n (S o)) where
  inverse (MkPiece x y) = MkPiece (inverse x) (inverse y)

public export
{n : Nat} -> {o : Nat} ->
  Generate (PVect n) g => Generate (Vect n (C (S o))) g =>
  Generate (Piece n (S o)) g where
  p <++> g = MkPiece (p.permutation <++> g) (permute p.permutation $ p.orientation <++> g)
