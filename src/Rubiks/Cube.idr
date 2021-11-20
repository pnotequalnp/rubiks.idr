module Rubiks.Cube

import public Data.Group
import Data.Group.Generics
import Data.Nat
import Data.Vect
import public Rubiks.Piece

%default total

||| The NxNxN Rubik's cube. A slight difference to physical cubes, Cube 0 corresponds to the 1x1x1
||| cube (which is trivial), while Cube 1 corresponds to an edges-only cube. This falls out of the
||| defintion used on its own, which is mildly interesting.
public export
record Cube (n : Nat) where
  constructor MkCube
  corners : Piece 8 3 ^ (min 2 n `minus` 1)
  midges : Piece 12 2 ^ (modNatNZ n 2 SIsNonZero)
  wings : Piece 24 1 ^ (n `minus` 3)
  centers : Piece 24 1 ^ (n `minus` 3) -- TODO: make it not a supercube for n > 3

public export
Show (Cube 0) where
  show _ = "()"

public export
Show (Cube 1) where
  show c = show $ index 0 c.midges

public export
Show (Cube 2) where
  show c = show $ index 0 c.corners

public export
Show (Cube 3) where
  show c = show (index 0 c.corners) <+> "\n" <+> show (index 0 c.midges)

public export
Eq (Cube n) where
  x == y =
    x.corners == y.corners
    && x.midges == y.midges
    && x.wings == y.wings
    && x.centers == y.centers

public export
Semigroup (Cube n) where
  x <+> y = MkCube
    (x.corners <+> y.corners)
    (x.midges <+> y.midges)
    (x.wings <+> y.wings)
    (x.centers <+> y.centers)

public export
{n : Nat} -> Monoid (Cube n) where
  neutral = MkCube neutral neutral neutral neutral

public export
{n : Nat} -> Group (Cube n) where
  inverse x = MkCube
    (inverse x.corners)
    (inverse x.midges)
    (inverse x.wings)
    (inverse x.centers)

public export
Generate (Piece 8 3) g => Generate (Cube 2) g where
  c <++> g = MkCube ((<++> g) <$> c.corners) [] [] []

public export
Generate (Piece 8 3) g => Generate (Piece 12 2) g => Generate (Cube 3) g where
  c <++> g = MkCube ((<++> g) <$> c.corners) ((<++> g) <$> c.midges) [] []
