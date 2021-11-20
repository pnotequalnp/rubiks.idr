module Data.Group.Permutation.Vect

import public Data.Group.Permutation
import Data.Vect
import Data.Vect.Extra

%default total

public export
record S (n : Nat) where
  constructor MkS
  permutation : Vect n (Fin n)

public export
Show (S n) where
  show p = show p.permutation

permute : S n -> Vect n a -> Vect n a
permute p xs = mapWithIndex go xs
  where
  go : Fin n -> a -> a
  go ix x =
    let jx = index ix p.permutation
     in index jx xs

public export
toCycles : S n -> Cycles n

public export
Eq (S n) where
  (==) = (==) `on` permutation

public export
Semigroup (S n) where
  p <+> q = record { permutation $= permute q } p

public export
{n : Nat} -> Monoid (S n) where
  neutral = MkS idVect

public export
{n : Nat} -> Group (S n) where
  inverse p = MkS $ ?s
  where
    indices : Vect n (Fin n)
    indices = mapWithIndex (\i, j => ?r) p.permutation

public export
{n : Nat} -> Permutation S n where
  permute = Data.Group.Permutation.Vect.permute
  asVect = permutation
