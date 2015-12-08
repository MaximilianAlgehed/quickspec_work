{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

isert :: Int -> [Int] -> [Int]
isert x [] = [x]
isert x (y:ys) 
    | x > y = y:(isert x ys)
    | otherwise = x:y:ys

sig =
  signature {
    maxTermSize = Just 7,
    constants = [
       constant "insert" (isert :: Int -> [Int] -> [Int]),
       constant "[]" ([] :: [Int]),
       constant ":" ((:) :: Int -> [Int] -> [Int]),
       constant "max" (max :: Int -> Int -> Int),
       constant "min" (min :: Int -> Int -> Int)
    ]
   }

main = quickSpec sig
