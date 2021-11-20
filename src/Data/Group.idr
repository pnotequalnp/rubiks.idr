module Data.Group

import Data.Vect

%default total

infixl 9 ^

public export
(^) : Type -> Nat -> Type
ty ^ n = Vect n ty

||| A group `g` is a monoid where every element has an inverse element
||| such that `g1 <+> g2 == g2 <+> g1 == neutral`.
public export
interface Monoid g => Group g where
  constructor MkGroup
  inverse : g -> g

infixl 8 <++>

||| `h` is isomorphic to a subset of `g` and thus sets of `h`s can be
||| used to generate subgroups of `g`.
public export
interface Group g => Generate g h where
  public export
  (<++>) : g -> h -> g

||| A group is isomorphic to a subset of itself.
public export
Group g => Generate g g where
  (<++>) = (<+>)

||| The trivial group.
public export
Group () where
  inverse = id

public export
{n : Nat} -> Group g => Group (Vect n g) where
  inverse = map inverse
