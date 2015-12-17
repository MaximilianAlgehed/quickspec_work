{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

partial :: Int -> Int -> Int
partial x y 
    | x >= y = x

sig =
  signature {
    maxTermSize = Just 6,
    constants = [
       constant "max" (max :: Int -> Int -> Int),
       constant "min" (min :: Int -> Int -> Int),
       constant "partial" (partial :: Int -> Int -> Int)
    ]
   }

main = quickSpec sig
