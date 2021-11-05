module Test

import Data.Group.Permutation
import Data.List1
import Data.Vect

foo : S 6
foo = FromCycles [1 ::: [2]]

stuff : Vect 6 Int
stuff = [8, 7, 6, 5, 4, 3]

stuff' : Vect 6 Int
stuff' = permute foo stuff

main : IO ()
main = do
  printLn $ toVect foo
  printLn stuff'
  putStrLn "tests passed"
