{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

partial :: Int -> Int -> Int
partial x y 
    | x >= y = x

predicate :: Int -> Int -> (Int, Int)
predicate x y 
    | x >= y = (x, y)
    | otherwise = (1, 0)

sig =
  signature {
    maxTermSize = Just 10,
    constants = [
       constant "fstp" ((fst .) . predicate :: Int -> Int -> Int),
       constant "sndp" ((snd .) . predicate :: Int -> Int -> Int),
       constant "partial" (partial :: Int -> Int -> Int)
    ]
   }

main = quickSpec sig

prop_partial :: Int -> Int -> Bool
prop_partial x y = partial x' y' == x'
    where
        x' = fst (predicate x y)
        y' = snd (predicate x y)
