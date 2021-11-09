module Data.Group.Permutation.Vect

import public Data.Group.Permutation
import Data.Vect
import Data.Vect.Extra

%default total

public export
record PVect (n : Nat) where
  constructor MkPVect
  permutation : Vect n (Fin n)

public export
Show (PVect n) where
  show p = show p.permutation

permute : PVect n -> Vect n a -> Vect n a
permute p xs = mapWithIndex go xs
  where
  go : Fin n -> a -> a
  go ix x =
    let jx = index ix p.permutation
     in index jx xs

public export
toCycles : PVect n -> Cycles n

public export
Eq (PVect n) where
  (==) = (==) `on` permutation

public export
Semigroup (PVect n) where
  p <+> q = record { permutation $= permute q } p

public export
{n : Nat} -> Monoid (PVect n) where
  neutral = MkPVect idVect

public export
{n : Nat} -> Group (PVect n) where
  inverse = id -- TODO

public export
{n : Nat} -> Permutation PVect n where
  permute = Data.Group.Permutation.Vect.permute
  asVect = permutation
