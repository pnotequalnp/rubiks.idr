module Data.Group.Generics

import Data.Group
import Data.SOP
import Generics.Derive

%default total

public export %hint
groupToMonoidNP : NP (Group . f) ks -> NP (Monoid . f) ks
groupToMonoidNP = mapNP (\_ => materialize Monoid)

public export
(all : NP (Group . f) ks) => Group (NP_ k f ks) where
  inverse {all = []}     []       = []
  inverse {all = _ :: _} (h :: t) = inverse h :: inverse t

public export %hint
groupToMonoidPOP : POP (Group . f) kss -> POP (Monoid . f) kss
groupToMonoidPOP = mapPOP (\_ => materialize Monoid)

public export
POP (Group . f) kss => Group (POP_ k f kss) where
  inverse (MkPOP np) = MkPOP $ inverse np

public export
(all : NP (Group . f) [k']) => Group (NS_ k f [k']) where
  inverse {all = _ :: _} (Z x) = Z $ inverse x
  inverse {all = _ :: _} (S _) impossible

public export
POP (Group . f) [ks] => Group (SOP_ k f [ks]) where
  inverse (MkSOP x) = MkSOP $ inverse x

public export
genInverse : Generic t [ts] => POP Group [ts] => t -> t
genInverse = to . inverse . from

export
GroupVis : Visibility -> DeriveUtil -> InterfaceImpl
GroupVis vis g = MkInterfaceImpl "Group" vis []
  `(MkGroup genInverse)
  (implementationType `(Group) g)

export
Group : DeriveUtil -> InterfaceImpl
Group = GroupVis Public
