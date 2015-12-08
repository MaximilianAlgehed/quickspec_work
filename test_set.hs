{-# LANGUAGE DeriveDataTypeable #-}
import Data.List hiding (union)
import Test.QuickCheck
import QuickSpec

data Different = D {a::Int, b::Int} deriving (Eq, Ord, Typeable)

instance Arbitrary Different where
    arbitrary =
        do
            x <- arbitrary
            y <- arbitrary `suchThat` (x/=)
            return (D x y)

type Set = [Int]

union :: Set -> Set -> Set
union xs ys = nub (xs ++ ys)

singleton :: Int -> Set
singleton x = [x]

empty :: Set
empty = []

sig =
  signature {
    maxTermSize = Just 10,
    instances = [
                 baseType (undefined::Different),
                 names (NamesFor ["q"] :: NamesFor Different)
                ],
    constants = [
       constant "empty" (empty :: Set),
       constant "union" (union :: Set -> Set -> Set),
       constant "singleton" (singleton :: Int -> Set),
       constant "a" (a :: Different -> Int),
       constant "b" (b :: Different -> Int)
    ]
   }

main = quickSpec sig
