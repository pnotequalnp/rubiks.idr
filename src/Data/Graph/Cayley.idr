module Data.Graph.Cayley

import Data.Enum
import Data.Graph
import Data.Group
import Data.List.Lazy

%default total

||| The Cayley graph of a generating set.
public export
Enum h => Generate g h => Graph g h where
  neighbors g = (\h => (h, g <++> h)) <$> fromList enum

||| Cayley graph with explicit list of generators.
public export
cayley : Generate g h => List h -> Graph g h
cayley generators = MkGraph neighbors
  where
  neighbors : g -> LazyList (h, g)
  neighbors g = (\h => (h, g <++> h)) <$> fromList generators
