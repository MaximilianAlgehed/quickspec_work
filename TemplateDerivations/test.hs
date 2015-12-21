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

lt :: Int -> Int -> Bool
lt = (<)

$(mk_Predicates [[| lt :: Int -> Int -> Bool |], [| gt :: Int -> Int -> Bool |]])

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

instance Predicateable2 Plt Plt where
    predicate2 p1 p2 = (a22 p1) == (a21 p2)

sig =
  signature {
    maxTermSize = Just 7,
    instances = [
                 baseType (undefined::Pgt),
                 names (NamesFor ["p"] :: NamesFor Pgt),
                 baseType (undefined::Plt),
                 names (NamesFor ["p'"] :: NamesFor Plt),
                 baseType (undefined::Predicate2 Plt Plt),
                 names (NamesFor ["q"] :: NamesFor (Predicate2 Plt Plt))
                ],
    constants = [
       constant "isort" (isort :: [Int] -> [Int]),
       constant "isert" (isert :: Int -> [Int] -> [Int]),
       constant "[]" ([] :: [Int]),
       constant ":" ((:) :: Int -> [Int] -> [Int]),
       constant "T" (True :: Bool),
       constant "<" ((<) :: Int -> Int -> Bool),
       constant "x" (coerce . a21 :: Pgt -> Int),
       constant "y"  (coerce . a22 :: Pgt -> Int),
       constant "x'" (coerce . a21 :: Plt -> Int),
       constant "y'" (coerce . a22 :: Plt -> Int),
       constant "p1" (coerce . a21 :: Predicate2 Plt Plt -> Plt),
       constant "p2" (coerce . a22 :: Predicate2 Plt Plt -> Plt)
    ]
   }

main = quickSpec sig
