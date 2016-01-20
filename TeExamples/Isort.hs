{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import TemplateDerivingPredicates
import Test.QuickCheck
import QuickSpec
import Data.Coerce

gt :: Int -> Int -> Bool
gt = (>)

$(mk_Predicates [
                 [| gt :: Int -> Int -> Bool |]
                ])

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
    instances = [
                 baseType (undefined::Pgt),
                 names (NamesFor ["p"] :: NamesFor Pgt)
                ],
    constants = [
       constant "isort" (isort :: [Int] -> [Int]),
       constant "isert" (isert :: Int -> [Int] -> [Int]),
       constant "[]" ([] :: [A]),
       constant ":" ((:) :: A -> [A] -> [A]),
       constant "x" (coerce . a21 :: Pgt -> Int),
       constant "y"  (coerce . a22 :: Pgt -> Int)
    ]
   }

main = quickSpec sig
