module Data.Enum

%default total

public export
interface Enum a where
  enum : List a

public export
Enum () where
  enum = [()]
