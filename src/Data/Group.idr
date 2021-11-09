module Data.Group

%default total

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

public export
Group () where
  inverse = id
