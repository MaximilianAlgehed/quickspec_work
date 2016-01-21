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

newtype Lst = Lst [Int] deriving (Ord, Eq, Typeable, Arbitrary)

sorted_ :: Lst -> Bool
sorted_ = isSorted . (coerce :: Lst -> [Int])

$(mk_Predicates [
                 [| gt :: Int -> Int -> Bool |],
                 [| lt :: Int -> Int -> Bool |],
                 [| sorted_ :: Lst -> Bool |]
                ])

eq :: Plt -> Plt -> Bool
eq p1 p2 = (a22 p1) == (a21 p2)

eq_ :: Pgt -> Pgt -> Bool
eq_ p1 p2 = (a22 p1) == (a21 p2)

$(mk_Predicates [
                [| eq :: Plt -> Plt -> Bool |],
                [| eq_ :: Pgt -> Pgt -> Bool |]
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
    maxTermSize = Just 12,
    instances = [
                 baseType (undefined::Pgt),
                 names (NamesFor ["p"] :: NamesFor Pgt),
                 baseType (undefined::Plt),
                 names (NamesFor ["p'"] :: NamesFor Plt),
                 baseType (undefined::Peq),
                 names (NamesFor ["q'"] :: NamesFor Peq)
 --                baseType (undefined::Peq_),
--                 names (NamesFor ["q"] :: NamesFor Peq_)
--                 baseType (undefined::Psorted_),
--                 names (NamesFor ["ps"] :: NamesFor Psorted_),
--                 baseType (undefined::Lst),
--                 names (NamesFor ["is", "js", "ks"] :: NamesFor Lst)
                ],
    constants = [
       --constant "isort" (Lst . isort . coerce :: Lst -> Lst),
 --      constant "isert" ((\x xs -> Lst (isert x (coerce xs))) :: Int -> Lst -> Lst),
 --      constant "[]" (Lst [] :: Lst),
       --constant ":" ((\x xs -> Lst (x:(coerce xs))) :: Int -> Lst -> Lst),
       constant "T" (True :: Bool),
       constant "<" ((<) :: Int -> Int -> Bool),
       constant "xs" (coerce . a11 :: Psorted_ -> Lst),
       --constant "x" (coerce . a21 :: Pgt -> Int),
       --constant "y"  (coerce . a22 :: Pgt -> Int),
       constant "sorted" (sorted_ :: Lst -> Bool)
       ,
       constant "x'" (coerce . a21 :: Plt -> Int),
       constant "y'" (coerce . a22 :: Plt -> Int),
       constant "p1'" (coerce . a21 :: Peq -> Plt),
       constant "p2'" (coerce . a22 :: Peq -> Plt)
--       constant "p1" (coerce . a21 :: Peq_ -> Pgt),
--       constant "p2" (coerce . a22 :: Peq_ -> Pgt)  
    ]
   }

main = quickSpec sig
