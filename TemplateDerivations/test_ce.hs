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
isert :: Int -> [Int] -> [Int]
isert x [] = [x]
isert x (y:ys) 
    | x > y = y:(isert x ys)
    | otherwise = x:y:ys

-- Insetion sort
isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = isert x (isort xs)

whenGreater :: (Int, Int, Pgt) -> (Int, Int)
whenGreater (x, y, z)
    | x > y = (x, y)
    | otherwise = ((coerce . a21) z, (coerce . a22) z)

sig =
  signature {
    maxTermSize = Just 12,
    instances = [
                 baseType (undefined::(Int, Int, Pgt)),
                 names (NamesFor ["p"] :: NamesFor (Int, Int, Pgt))
                ],
    constants = [
       constant "isort" (isort :: [Int] -> [Int]),
       constant "isert" (isert :: Int -> [Int] -> [Int]),
       constant "[]" ([] :: [Int]),
       constant ":" ((:) :: Int -> [Int] -> [Int]),
       constant "x" (fst . whenGreater),
       constant "y" (snd . whenGreater)
    ]
   }

main = quickSpec sig
