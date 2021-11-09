module Rubiks

import Data.Enum
import Data.Graph
import Data.Graph.Cayley
import Data.Group
import Data.List.Lazy
import Rubiks.Cube
import Rubiks.Metric.RUF

%default total

scramble : List RUF
scramble = [U, R', F2, U']

scrambled : Cube 3
scrambled = foldl (<++>) neutral scramble

solution : Maybe (List RUF)
solution = head' $ ids neutral scrambled 4
