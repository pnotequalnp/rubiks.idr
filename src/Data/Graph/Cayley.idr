module Data.Graph.Cayley

import Data.Graph
import Data.Group
import Data.List.Lazy
import Generics.SOP

%default total

||| Cayley graph with explicit list of generators.
public export
cayley' : Generate g h => List h -> Graph g h
cayley' generators = MkGraph $ \g => (\h => (h, g <++> h)) <$> fromList generators

||| The Cayley graph of a generating set.
public export %hint
cayley : Generic h code => EnumType code => Generate g h => Graph g h
cayley = cayley' values
