{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

import TemplateDerivingPredicates
import Test.QuickCheck
import QuickSpec
import Data.Coerce

$(mk_Predicate_Derivations 2)

newtype IntWrapper = IntWrapper Int deriving (Eq, Ord, Typeable)

instance Predicateable2 IntWrapper IntWrapper where
    predicate2 (IntWrapper x) (IntWrapper y) = x > y

instance Arbitrary IntWrapper where
    arbitrary = return . IntWrapper =<< arbitrary

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
    maxTermSize = Just 7,
    instances = [
                 baseType (undefined::Predicate2 IntWrapper IntWrapper),
                 names (NamesFor ["p"] :: NamesFor (Predicate2 IntWrapper IntWrapper))
                ],
    constants = [
       constant "isort" (isort :: [Int] -> [Int]),
       constant "isert" (isert :: Int -> [Int] -> [Int]),
       constant "[]" ([] :: [Int]),
       constant ":" ((:) :: Int -> [Int] -> [Int]),
       constant "x" (coerce . a21 :: (Predicate2 IntWrapper IntWrapper) -> Int),
       constant "y" (coerce . a22 :: (Predicate2 IntWrapper IntWrapper) -> Int)
    ]
   }

main = quickSpec sig
