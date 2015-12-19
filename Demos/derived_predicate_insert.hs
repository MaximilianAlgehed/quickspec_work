{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import Data.Coerce
import QuickSpec

-- Class of arity 2 predicates
class (Predicatable2 a b) where
    -- The "user-input" predicate
    predicate2 :: (Coercible a a', Coercible b b') => a' -> b' -> Bool

    -- What is actually used internally
    getPredicate2 :: (Coercible a a', Coercible b b') => a -> b -> Bool
    getPredicate2 = (\a b -> predicate (coerce a :: a') (coerce b :: b'))

-- A predicate type for arity 2 predicates
data Predicate2 a b = P {x::(Coercible a a') => a', y::(Coercible b b') => b'} deriving (Ord, Eq, Typeable)

-- The general instance for arbitrary predicates of size 2
instance (Predicatable2 a b, Arbitrary a, Arbitrary b) => Arbitrary (Predicate2 a b) where
    arbitrary = do
                    -- Generate the touple all as one, that way it can be easily generalized to
                    -- arity N predicates
                    (x, y) <- arbitrary `suchThat` (\(x, y) -> getPredicate2 x y)
                    return (P x y)

-- A type wrapping ints 
newtype IntWrapper = IntWrapper Int deriving (Ord, Eq, Typeable)

-- The predicateable instance
instance Predicatable2 IntWrapper IntWrapper where
    predicate2 = (>) :: (Int -> Int -> Bool)

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
            constant "x" (x :: Predicate2 IntWrapper IntWrapper -> Int),
            constant "y" (y :: Predicate2 IntWrapper IntWrapper -> Int)
        ]
   }

main = quickSpec sig
