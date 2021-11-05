module Data.Graph.Cayley

import Data.Graph
import Data.Group
import Data.List.Lazy

%default total

public export
(gens : List h) => Generate g h => Graph g h where
  neighbors g = (\h => (h, g <++> h)) <$> fromList gens

public export
cayley : Generate g h => List h -> { default (\g1, g2 => 0) bound : g -> g -> Nat } -> Graph g h
cayley generators = MkGraph neighbors bound
  where
  neighbors : g -> LazyList (h, g)
  neighbors g = (\h => (h, g <++> h)) <$> fromList generators
