{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

data Greater = G {x::Int , y::Int} deriving (Eq, Ord, Typeable)
data GreaterOrEqual = GOE {x'::Int, y'::Int} deriving (Eq, Ord, Typeable)
data SList = SL {as::[Int]} deriving (Eq, Ord, Typeable)
           
instance Arbitrary GreaterOrEqual where
    arbitrary = 
        do
            x' <- arbitrary
            y' <- arbitrary `suchThat` (x' >=)
            return (GOE x' y')
    shrink (GOE a b) = [GOE a' b' | b' <- [b, b-1..], a' <- [a, a-1..], a' >= b'] 

instance Arbitrary Greater where
    arbitrary =
        do
            x <- arbitrary
            y <- arbitrary `suchThat` (x>)
            return (G x y)

    shrink (G a b) = [G a' b' | b' <- [b, b-1..], a' <- [a, a-1..], a' > b'] 

instance Arbitrary SList where
    arbitrary = return . SL =<< arbitrary `suchThat` isSorted

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
                 names (NamesFor ["p'"] :: NamesFor GreaterOrEqual),
                 baseType (undefined::SList),
                 names (NamesFor ["qs"] :: NamesFor SList)

                ],
    constants = [
       constant "insert" (isert :: Int -> [Int] -> [Int]),
       constant "[]" ([] :: [Int]),
       constant ":" ((:) :: Int -> [Int] -> [Int]),
       constant "x" (x :: Greater -> Int),
       constant "y" (y :: Greater -> Int)
    ]
   }

main = quickSpec sig
