module Data.Group

import Data.DPair
import Data.Vect

%default total

infixl 9 ^

public export
(^) : Type -> Nat -> Type
ty ^ n = Vect n ty

||| A group g is a monoid where every element has an inverse element such that g1 <+> g2 == g2 <+>
||| g1 == neutral.
public export
interface Monoid g => Group g where
  constructor MkGroup
  inverse : g -> g

infixl 8 <++>

||| h is isomorphic to a subset of g and thus sets of hs can be used to generate subgroups of g. h
||| need not be isomorphic to a subgroup of g.
public export
interface Group g => Generate g h where
  public export
  (<++>) : g -> h -> g
  fold : Foldable t => t h -> g
  fold = foldl (<++>) neutral

||| A group is isomorphic to a subset of itself.
public export
Group g => Generate g g where
  (<++>) = (<+>)

||| The unit type is isomorphic to the singleton subset of the identity element.
public export
Group g => Generate g () where
  x <++> () = x

||| General subsets.
public export
Group g => Generate g (Subset g p) where
  x <++> y = x <+> y.fst

||| The trivial group.
public export
Group () where
  inverse = id

||| The homogeneous product of groups.
public export
{n : Nat} -> Group g => Group (g ^ n) where
  inverse = map inverse
