module Data.Graph

import Data.List.Lazy

%default total

||| Representation of a graph as a function from a vertex to its neighbors.
public export
interface Graph v e where
  constructor MkGraph
  neighbors : v -> LazyList (e, v)

||| Compute all non-redundant (i.e. not passing through the goal vertex) paths shorter
||| than the given upper bound with a depth-first search. Accepts a function providing
||| a lower bound to prune the search tree.
public export
dfs : Graph v e => Eq v => {default (const 0) bound : v -> Nat} -> v -> v -> Nat -> LazyList (List e)
dfs goal = go []
  where
  go : List e -> v -> Nat -> LazyList (List e)
  go path start max with (start == goal, bound start <= max)
    go path _ _ | (True, _) = [path]
    go path start 0 | (False, _) = []
    go path start (S n) | (False, False) = neighbors start >>= (\(e, v) => go (e :: path) v n)
    go path start (S n) | (False, _) = []

||| Compute all non-redundant (i.e. not passing through the goal vertex) paths shorter
||| than the given upper bound with a iterated deepening depth-first search. Accepts a
||| function providing a lower bound to prune the search tree, making this algorithm
||| IDA*.
public export
ids : Graph v e => Eq v => {default (const 0) bound : v -> Nat} -> v -> v -> Nat -> LazyList (List e)
ids goal start n = fromList [0 .. n] >>= dfs {bound} goal start
