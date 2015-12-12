{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = 
    (quicksort [y | y <- xs, y < x])
    ++ [x] ++
    (quicksort [y | y <- xs, y >= x])

sig =
  signature {
    maxTermSize = Just 7,
    constants = [
       constant "quicksort" (quicksort :: [Int] -> [Int]),
       constant "[]" ([] :: [Int]),
       constant ":" ((:) :: Int -> [Int] -> [Int])
    ]
   }

prop_qs i j is = quicksort (i:j:is) == quicksort (j:i:is)

main = quickSpec sig
