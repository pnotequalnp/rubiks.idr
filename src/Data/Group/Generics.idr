module Data.Group.Generics

import Generics.Derive
import Data.Group

%default total
%language ElabReflection

public export
Monoid (NP f ks) => NP (Group . f) ks => Group (NP f ks) where
  inverse = hcmap (Group . f) inverse

public export
Monoid (NS f ks) => NP (Group . f) ks => Group (NS f ks) where
  inverse = hcmap (Group . f) inverse

public export
Monoid (POP f kss) => POP (Group . f) kss => Group (POP f kss) where
  inverse = hcmap (Group . f) inverse

public export
Monoid (SOP f kss) => SOP (Group . f) kss => Group (SOP f kss) where
  inverse = ?inv

public export
genInverse : Generic t code
          => Group (SOP I code)
          => t -> t
genInverse = to . inverse . from

public export
GroupVis : Visibility -> DeriveUtil -> InterfaceImpl
GroupVis vis g = MkInterfaceImpl "Group" vis []
  `(MkGroup genInverse)
  (implementationType `(Group) g)

public export
Group : DeriveUtil -> InterfaceImpl
Group = GroupVis Public

record DoubleUnit where
  constructor MkDouble
  u1, u2 : ()

%runElab derive "DoubleUnit" [Generic, Semigroup, Monoid, Group]
