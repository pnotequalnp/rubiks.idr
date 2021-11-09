module Rubiks

import Data.Graph
import Data.Graph.Cayley
import Data.Group
import Data.List.Lazy
import Generics.SOP
import Rubiks.Cube
import Rubiks.Metric.RUF

%default total

scramble : List RUF
scramble = [U, R', F2, U']

scrambled : Cube 3
scrambled = foldl (<++>) neutral scramble

export
solution : Maybe (List RUF)
solution = head' $ ids @{cayley} neutral scrambled 8
