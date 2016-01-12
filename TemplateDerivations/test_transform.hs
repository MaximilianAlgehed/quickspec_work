{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Debug.Trace
import TemplateDerivingFunctions
import QuickSpec
import Test.QuickCheck

isPalindrome :: [Int] -> Bool
isPalindrome xs = xs == (reverse xs)

$(mk_When_Function [| isPalindrome :: [Int] -> Bool |] [| [] |])

sig =
  signature {
    maxTermSize = Just 12,
    constants = [
       constant "P" when_isPalindrome,
       constant "reverse" (reverse :: [Int] -> [Int]),
       constant "len1" ((\x -> (length x) <= 1) :: [Int] -> Bool),
       constant "T" True
    ]
   }

main = quickSpec sig
