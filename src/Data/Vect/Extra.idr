module Data.Vect.Extra

import Data.Vect

%default total

public export
mapWithIndex : (Fin n -> a -> b) -> Vect n a -> Vect n b
mapWithIndex f = \case
  [] => []
  x :: xs => f FZ x :: mapWithIndex (f . FS) xs

public export
idVect : {n : Nat} -> Vect n (Fin n)
idVect = mapWithIndex const $ replicate n ()
