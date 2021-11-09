module Data.Group

import Data.Vect

%default total

||| A group `g` is a monoid where every element has an inverse element
||| such that `g1 <+> g2 == g2 <+> g1 == neutral`.
public export
interface Monoid g => Group g where
  constructor MkGroup
  inverse : g -> g

infixl 8 <++>

||| Types `h` which a list thereof can generate a subgroup of `g`.
public export
interface Group g => Generate g h where
  public export
  (<++>) : g -> h -> g

||| A subset of a group can generate subgroups of that group itself.
public export
Group g => Generate g g where
  (<++>) = (<+>)

||| The trivial group.
public export
Group () where
  inverse = id

public export
(n : Nat) => Group g => Group (Vect n g) where
  inverse = map inverse
