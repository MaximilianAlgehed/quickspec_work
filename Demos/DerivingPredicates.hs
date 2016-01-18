{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module DerivingPredicates where

import Test.QuickCheck
import Data.Coerce
import QuickSpec

-- Class of arity 2 predicates
class (Predicatable2 a b) where
    predicate2 :: a -> b -> Bool

-- A predicate type for arity 2 predicates
data Predicate2 a b = P {a21::a, a22::b} deriving (Ord, Eq, Typeable)

-- The general instance for arbitrary predicates of size 2
instance (Predicatable2 a b, Arbitrary a, Arbitrary b) => Arbitrary (Predicate2 a b) where
    arbitrary = do
                    -- Generate the touple all as one, that way it can be easily generalized to
                    -- arity N predicates
                    (x, y) <- arbitrary `suchThat` (\(x, y) -> predicate2 x y)
                    return (P x y)
