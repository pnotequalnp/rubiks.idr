module Data.Graph

import Data.List.Lazy

%default total

public export
interface Graph v e where
  constructor MkGraph
  neighbors : v -> LazyList (e, v)
  lowerBound : v -> v -> Nat
  lowerBound _ _ = 0

public export
dfs : Graph v e => Eq v => v -> v -> Nat -> LazyList (List e)
dfs goal = go []
  where
  go : List e -> v -> Nat -> LazyList (List e)
  go path start bound with (start == goal)
    go path _ _ | True = [path]
    go path start 0 | False = []
    go path start (S n) | False = neighbors start >>= (\(e, v) => go (e :: path) v n)

public export
ids : Graph v e => Eq v => v -> v -> Nat -> LazyList (List e)
ids goal start n = fromList [0 .. n] >>= dfs goal start
