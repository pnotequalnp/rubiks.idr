module Data.Group

import Generics.Derive

%default total
%language ElabReflection

public export
interface Monoid g => Group g where
  constructor MkGroup
  inverse : g -> g

infixl 8 <++>

public export
interface Group g => Generate g h where
  public export
  (<++>) : g -> h -> g

public export
Group g => Generate g g where
  (<++>) = (<+>)

Group () where
  inverse = id

public export
Monoid (NP f ks) => NP (Group . f) ks => Group (NP f ks) where
  inverse = hcmap (Group . f) inverse

public export
Monoid (NS f ks) => NP (Group . f) ks => SingletonList ks => Group (NS f ks) where
  inverse = hcmap (Group . f) inverse

public export
Monoid (POP f kss) => POP (Group . f) kss => Group (POP f kss) where
  inverse (MkPOP nps) = MkPOP $ inverse nps

-- genInverse :  Generic t code
--           => POP Group code
--           => SingletonList code
--           => t -> t
-- genInverse = inverse . from

GroupVis : Visibility -> DeriveUtil -> InterfaceImpl
GroupVis vis g = MkInterfaceImpl "Group" vis []
                       `(MkGroup ?genInverse)
                       (implementationType `(Group) g)

Group' : DeriveUtil -> InterfaceImpl
Group' = GroupVis Public

record DoubleUnit where
  constructor MkDouble
  u1, u2 : ()

%runElab derive "DoubleUnit" [Generic, Semigroup, Monoid, Group']
