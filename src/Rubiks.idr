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
scramble : List RUF
scramble = [R, U, F', U2]

export
cube : Cube 3
cube = (neutral <++> R) <++> R'

export
scrambled : Cube 3
scrambled = foldl (<++>) neutral scramble

export
solution : Maybe (List RUF)
solution = map reverse . head' $ ids @{cayley} neutral scrambled 8

export
solve : Eq puzzle => Generic metric code => EnumType code => Generate puzzle metric => List metric -> LazyList (List metric)
solve scramble = ids @{cayley {g = puzzle, h = metric}} neutral (foldl (<++>) neutral scramble) 6

export
solve' : Eq puzzle => Generic metric code => EnumType code => Generate puzzle metric => List metric -> Maybe (List metric)
solve' = head' . solve {puzzle}

main : IO ()
main = pure ()
