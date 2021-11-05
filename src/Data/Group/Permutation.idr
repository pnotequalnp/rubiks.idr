module Data.Group.Permutation

import Data.Fin
import public Data.Group
import Data.Vect
import Data.Vect.Extra

%default total

public export
interface Group (p n) => Permutation p n where
  permute : p n -> Vect n a -> Vect n a
  asVect : p n -> Vect n (Fin n)
  -- asVect p = permute p idVect

public export
Cycle : Nat -> Type
Cycle n = List (Fin n)

cycle : Cycle n -> Vect n a -> Vect n a
cycle c v = case c of
  [] => v
  i :: ixs =>
    let (j, v') = go i ixs v
     in replaceAt j (index i v) v'
  where
  go : Fin n -> List (Fin n) -> Vect n a -> (Fin n, Vect n a)
  go i ixs xs = case ixs of
    [] => (i, xs)
    j :: ixs' => go j ixs' $ replaceAt i (index j xs) xs

public export
record Cycles (n : Nat) where
  constructor MkCycles
  cycles : List (Cycle n)

public export
Nil : Cycles n
Nil = MkCycles Nil

public export
(::) : Cycle n -> Cycles n -> Cycles n
(::) c = record { cycles $= (c ::) }

public export
(n : Nat) => Semigroup (Cycles n) where
  (<+>) p = record { cycles $= (p.cycles ++) }

public export
(n : Nat) => Monoid (Cycles n) where
  neutral = MkCycles []

public export
(n : Nat) => Group (Cycles n) where
  inverse = ?s

public export
(n : Nat) => Permutation Cycles n where
  permute p xs = foldl (flip cycle) xs p.cycles
  asVect p = permute p idVect

idVect : (n : Nat) -> Vect n Nat
idVect n = reverse $ go n
  where
  go : (m : Nat) -> Vect m Nat
  go m = case m of
    0 => []
    S m => m :: go m

public export
(n : Nat) => Eq (Cycles n) where
  (==) = (==) `on` asVect
