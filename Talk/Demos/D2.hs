{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import TemplateDerivingPredicates
import QuickSpec.PrintConditionally
import Test.QuickCheck
import QuickSpec
import Data.Coerce

gt :: Int -> Int -> Bool
gt = (>)

$(mk_Predicates [[| gt :: Int -> Int -> Bool |]])

isert :: Int -> [Int] -> [Int]
isert x [] = [x]
isert x (y:ys) 
    | x > y     = y:(isert x ys)
    | otherwise = x:y:ys

isort :: [Int] -> [Int]
isort []     = []
isort (x:xs) = isert x (isort xs)

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

main = do
        thy <- quickSpec sig
        printConditionally [(constant ">" ((>) :: Int -> Int -> Bool),
                            [constant "x" (coerce . a21 :: Pgt -> Int), constant "y"  (coerce . a22 :: Pgt -> Int)])
                           ] thy
