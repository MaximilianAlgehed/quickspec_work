{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

-- Insert
-- Precondition: arg2 is sorted
isert :: Int -> [Int] -> [Int]
isert x [] = [x]
isert x (y:ys) 
    | x > y = y:(isert x ys)
    | otherwise = x:y:ys

-- Insetion sort
isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = isert x (isort xs)

sig =
  signature {
    maxTermSize = Just 7,
    constants = [
       constant "isort" (isort :: [Int] -> [Int]),
       constant "isert" (isert :: Int -> [Int] -> [Int]),
       constant "[]" ([] :: [Int]),
       constant ":" ((:) :: Int -> [Int] -> [Int]),
       constant "max" (max :: Int -> Int -> Int)
    ]
   }

main = quickSpec sig
