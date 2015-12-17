{-# LANGUAGE DeriveDataTypeable #-}
import Data.List hiding (union)
import Test.QuickCheck 
import QuickSpec hiding (S)

newtype Set = S {members::[Int]} deriving (Ord, Typeable, Show)

instance Arbitrary Set where
    arbitrary = return . S . nub =<< arbitrary

instance Eq Set where
    (S xs) == (S ys) = (sort xs) == (sort ys)

data Disjoint = D {x::Set, y::Set} deriving (Ord, Typeable, Eq)

instance Arbitrary Disjoint where
    arbitrary = do
                    x <- arbitrary
                    y <- arbitrary
                    return (D x (S [z | z<-members y, not (z `setMember` x)]))

setMember :: Int -> Set -> Bool
setMember x (S xs) = elem x xs

union :: Set -> Set -> Set
union (S xs) (S ys) = S $ sort $ nub (xs ++ ys)

singleton :: Int -> Set
singleton x = S [x]

empty :: Set
empty = S []

setSize :: Set -> Int
setSize (S xs) = length xs

prop_union s1 s2 = (union s1 s2) == (union s2 s1)

sig =
  signature {
    maxTermSize = Just 10,
    instances = [
                 baseType (undefined::Disjoint),
                 names (NamesFor ["p"] :: NamesFor Disjoint),
                 baseType (undefined::Set),
                 names (NamesFor ["s"] :: NamesFor Set)
                ],
    constants = [
       constant "empty" (empty :: Set),
       constant "union" (union :: Set -> Set -> Set),
       constant "singleton" (singleton :: Int -> Set),
       constant "x" (x :: Disjoint -> Set),
       constant "y" (y :: Disjoint -> Set),
       constant "size" (setSize :: Set -> Int),
       constant "+" ((+) :: Int -> Int -> Int),
       constant "1" (1 :: Int),
       constant "0" (0 :: Int)
    ]
   }

main = quickSpec sig
