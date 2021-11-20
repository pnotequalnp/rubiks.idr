module Rubiks

import Data.Graph
import Data.Graph.Cayley
import Data.Group
import Data.List.Lazy
import Generics.SOP
import public Rubiks.Cube
import public Rubiks.Metric.RUF

%default total

export
solve : Eq puzzle => Generic metric code => EnumType code => Generate puzzle metric => List metric -> LazyList (List metric)
solve scramble = ids @{cayley {g = puzzle, h = metric}} neutral (foldl (<++>) neutral scramble) 6

export
solve' : Eq puzzle => Generic metric code => EnumType code => Generate puzzle metric => List metric -> Maybe (List metric)
solve' = head' . solve {puzzle}
