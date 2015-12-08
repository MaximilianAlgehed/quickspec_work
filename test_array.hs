{-# LANGUAGE DeriveDataTypeable #-}
import Data.Array
import Test.QuickCheck
import QuickSpec

data Different = D {a::Int, b::Int} deriving (Eq, Ord, Typeable)

instance Arbitrary Different where
    arbitrary =
        do
            x <- arbitrary
            y <- arbitrary `suchThat` (x/=)
            return (D x y)

sig =
  signature {
    maxTermSize = Just 10,
    instances = [
                 baseType (undefined::Different),
                 names (NamesFor ["q"] :: NamesFor Different)
                ],
    constants = [
       constant "a" (a :: Different -> Int),
       constant "b" (b :: Different -> Int)
    ]
   }

main = quickSpec sig
