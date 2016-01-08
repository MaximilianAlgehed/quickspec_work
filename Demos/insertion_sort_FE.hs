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

whenGT :: (Int, Int) -> (Int, Int)
whenGT (x, y)
    | x > y = (x, y)
    | otherwise = (1, 0)

sig =
  signature {
    maxTermSize = Just 7,
    constants = [
       constant "isort" (isort :: [Int] -> [Int]),
       constant "isert" (isert :: Int -> [Int] -> [Int]),
       constant "[]" ([] :: [Int]),
       constant ":" ((:) :: Int -> [Int] -> [Int]),
       constant "a" (fst . whenGT),
       constant "b" (snd . whenGT)
    ]
   }

main = quickSpec sig
