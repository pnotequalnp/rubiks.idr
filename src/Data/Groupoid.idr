module Data.Groupoid

%default total

interface Groupoid g where
  (<@>) : g -> Maybe g
  inverse : g -> g
  neutral : g
