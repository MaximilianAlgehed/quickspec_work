{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

whenSorted :: (Ord a) => [a] -> [a]
whenSorted xs
    | isSorted xs = xs
    | otherwise = []

sig =
  signature {
    maxTermSize = Just 8,
    constants = [
       constant "True" (True :: Bool),
       constant "False" (False :: Bool),
       constant "sorted" (isSorted :: [Int] -> Bool),
       constant "==" ((==) :: Int -> Int -> Bool),
       constant "[]" ([] :: [Int]),
       constant ":" ((:) :: Int -> [Int] -> [Int]),
       constant "max" (max :: Int -> Int -> Int),
       constant "whenSorted" (whenSorted :: [Int] -> [Int])
       ]
   }

main = quickSpec sig
