{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import Data.Coerce
import QuickSpec
import DerivingPredicates 

-- A type wrapping ints 
newtype IntWrapper = IntWrapper Int deriving (Ord, Eq, Typeable)

-- The predicateable instance
instance Predicatable2 IntWrapper IntWrapper where
    predicate2 = (>)

-- Arbitrary instance
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
            constant "x" (coerce . x :: Predicate2 IntWrapper IntWrapper -> Int),
            constant "y" (coerce . y :: Predicate2 IntWrapper IntWrapper -> Int)
        ]
    }

main = quickSpec sig
