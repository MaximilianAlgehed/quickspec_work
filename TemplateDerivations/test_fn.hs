{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Debug.Trace
import TemplateDerivingFunctions
import QuickSpec
import Test.QuickCheck

greater :: Int -> Int -> Bool
greater = (>)
$(mk_When_Function [| greater :: Int -> Int -> Bool |] [| (1, 0) |])

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
    maxTermSize = Just 10,
    constants = [
       constant "isort" (isort :: [Int] -> [Int]),
       constant "isert" (isert :: Int -> [Int] -> [Int]),
       constant "[]" ([] :: [Int]),
       constant ":" ((:) :: Int -> [Int] -> [Int]),
       constant "greater" greater,
       constant "True" True,
       constant "a" (when_greater1 :: (Int, Int) -> Int),
       constant "b" (when_greater2 :: (Int, Int) -> Int)
    ]
   }

main = quickSpec sig

prop_greater :: (Int, Int) -> Bool
prop_greater x = (when_greater1 x) > (when_greater2 x)
