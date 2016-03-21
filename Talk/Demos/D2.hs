{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import TemplateDerivingPredicates
import Test.QuickCheck
import QuickSpec
import Data.Coerce

-- For Template Haskell
gt :: Int -> Int -> Bool
gt = (>)

-- Automatically create the type encoding
$(mk_Predicates [[| gt :: Int -> Int -> Bool |]])

-- Insert
isert :: Int -> [Int] -> [Int]
isert x [] = [x]
isert x (y:ys) 
    | x > y     = y:(isert x ys)
    | otherwise = x:y:ys

-- Insetion sort
isort :: [Int] -> [Int]
isort []     = []
isort (x:xs) = isert x (isort xs)

-- QuickSpec signature
sig =
      signature {
        maxTermSize = Just 9,
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
