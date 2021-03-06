{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

data Greater = G {x::Int , y::Int} deriving (Eq, Ord, Typeable)
          
instance Arbitrary Greater where
    arbitrary =
        do
            x <- arbitrary
            y <- arbitrary `suchThat` (x>)
            return (G x y)
    shrink (G a b) = [G a' b' | b' <- [b, b-1..], a' <- [a, a-1..], a' > b'] 

partial :: Int -> Int -> Int
partial x y 
    | x > y = x

sig =
  signature {
    maxTermSize = Just 10,
    instances = [
                 baseType (undefined::Greater),
                 names (NamesFor ["p"] :: NamesFor Greater)
                ],
    constants = [
       constant "x" (x :: Greater -> Int),
       constant "y" (y :: Greater -> Int),
       constant "partial" (partial :: Int -> Int -> Int)
    ]
   }

main = quickSpec sig
