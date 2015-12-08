{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

data Greater = G {x::Int , y::Int} deriving (Eq, Ord, Typeable)
data GreaterOrEqual = GOE {x'::Int, y'::Int} deriving (Eq, Ord, Typeable)
           
instance Arbitrary GreaterOrEqual where
    arbitrary = 
        do
            x' <- arbitrary
            y' <- arbitrary `suchThat` (x' >=)
            return (GOE x' y')

instance Arbitrary Greater where
    arbitrary =
        do
            x <- arbitrary
            y <- arbitrary `suchThat` (x>)
            return (G x y)

isert :: Int -> [Int] -> [Int]
isert x [] = [x]
isert x (y:ys) 
    | x > y = y:(isert x ys)
    | otherwise = x:y:ys

sig =
  signature {
    maxTermSize = Just 10,
    instances = [
                 baseType (undefined::Greater),
                 names (NamesFor ["p"] :: NamesFor Greater),
                 baseType (undefined::GreaterOrEqual),
                 names (NamesFor ["p'"] :: NamesFor GreaterOrEqual)

                ],
    constants = [
--       constant "True" (True :: Bool),
--       constant "False" (False :: Bool),
       constant "insert" (isert :: Int -> [Int] -> [Int]),
       constant "[]" ([] :: [Int]),
       constant ":" ((:) :: Int -> [Int] -> [Int]),
       constant "x" (x :: Greater -> Int),
       constant "y" (y :: Greater -> Int)
 --      constant "sorted" (isSorted :: [Int] -> Bool)
--       constant "x'" (x' :: GreaterOrEqual -> Int),
--       constant "y'" (y' :: GreaterOrEqual -> Int)
    ]
   }

main = quickSpec sig
