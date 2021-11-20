module Rubiks.Piece

import Data.Group
import Data.Group.Cyclic
import Data.Group.Permutation
import Data.Group.Permutation.Vect
import Data.Vect
import Generics.Derive

%default total
%language ElabReflection

||| The semidirect product Sₙ ⋉ Cₘⁿ, representing a single class of piece of a twisty puzzle,
||| consisting of n pieces with m orientations each.
public export
record Piece (n : Nat) (m : Nat) where
  constructor MkPiece
  permutation : S n
  orientation : C m ^ n

%runElab derive "Piece" [Generic, Meta]

public export
Show (Piece n (S m)) where
  show (MkPiece p o) = show p <+> "\n" <+> show o

public export
Eq (Piece n m) where
  MkPiece p1 o1 == MkPiece p2 o2 = p1 == p2 && o1 == o2

public export
{n : Nat} -> {m : Nat} -> Semigroup (Piece n (S m)) where
  MkPiece p1 o1 <+> MkPiece p2 o2 = let p' = p1 <++> p2
    in MkPiece p' $ permute p' (permute (inverse p1) o1 <+> o2)

public export
{n : Nat} -> {m : Nat} -> Monoid (Piece n (S m)) where
  neutral = MkPiece neutral neutral

public export
{n : Nat} -> {m : Nat} -> Group (Piece n (S m)) where
  inverse (MkPiece p o) = MkPiece (inverse p) (inverse o)

public export
{n : Nat} -> {m : Nat} ->
  Generate (S n) g => Generate (C (S m) ^ n) g =>
  Generate (Piece n (S m)) g where
  MkPiece p o <++> g = let permute = permute (neutral {ty = S n} <++> g)
    in MkPiece (p <++> g) (permute (o <++> g))
