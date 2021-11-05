module Data.Group.Cyclic

import Data.Fin
import Data.Group
import Data.Maybe
import Data.Nat

%default total

export
record C (n : Nat) where
  constructor FromNat
  nat : Nat
  0 bounded : LT nat n

modLT : (n : Nat) -> (m : Nat) -> LT (modNatNZ n (S m) SIsNonZero) (S m)
modLT = ?lt

(n : Nat) => Semigroup (C (S n)) where
  x <+> y = let z = x.nat + y.nat
    in FromNat (modNatNZ z (S n) SIsNonZero) $ modLT z n

(n : Nat) => Monoid (C (S n)) where
  neutral = FromNat 0 $ LTESucc LTEZero

minusLTE : (n : Nat) -> (m : Nat) -> LTE (n `minus` m) n
minusLTE = ?lte

(n : Nat) => Group (C (S n)) where
  inverse x = case x.nat of
    0 => neutral
    (S m) => FromNat (S n `minus` S m) $ LTESucc (minusLTE n m)

public export
fromInteger : (x : Integer) -> {n : Nat} ->
              {auto 0 prf : IsJust (maybeLT (fromInteger x) n)} ->
              C n
fromInteger x = FromNat (fromInteger x) $ fromJust (maybeLT (fromInteger x) n)
