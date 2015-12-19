{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import Data.Coerce
import QuickSpec

class (Predicatable2 a b) where
    getPredicate2 :: a -> b -> Bool

data Predicate2 a b = P {x::a, y::b} deriving (Ord, Eq, Typeable)

instance (Predicatable2 a b, Arbitrary a, Arbitrary b) => Arbitrary (Predicate2 a b) where
    arbitrary = do
                    (x, y) <- arbitrary `suchThat` (\(x, y) -> getPredicate2 x y)
                    return (P x y)

newtype IntWrapper = IntWrapper Int deriving (Ord, Eq, Typeable)

predicate :: IntWrapper -> IntWrapper -> Bool
predicate x y = (coerce x :: Int) > (coerce y :: Int)

instance Predicatable2 IntWrapper IntWrapper where
    getPredicate2 = predicate

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
