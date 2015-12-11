{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

-- A datatype that models the relationship x > y
data Greater = G {x::Int , y::Int} deriving (Eq, Ord, Typeable)
           
-- Generate an arbitrary Greater pair
instance Arbitrary Greater where
    arbitrary = 
        do
            x' <- arbitrary
            y' <- arbitrary `suchThat` (x'>)
            return (G x' y')

    shrink (G a b) = [G a' b' | b' <- [b, b-1..], a' <- [a, a-1..], a' >= b'] 

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
                 baseType (undefined::Greater),
                 names (NamesFor ["p"] :: NamesFor Greater),
                ],
    constants = [
       constant "isort" (isort :: [Int] -> [Int]),
       constant "isert" (isert :: Int -> [Int] -> [Int]),
       constant "[]" ([] :: [Int]),
       constant ":" ((:) :: Int -> [Int] -> [Int]),
       constant "x" (x :: Greater -> Int),
       constant "y" (y :: Greater -> Int)
    ]
   }

main = quickSpec sig
