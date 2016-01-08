{-# LANGUAGE DeriveDataTypeable #-}
import Data.List hiding (union)
import Test.QuickCheck 
import QuickSpec hiding (S)

newtype Set = S {members::[Int]} deriving (Ord, Typeable, Show)

instance Arbitrary Set where
    arbitrary = return . S . nub =<< arbitrary

instance Eq Set where
    (S xs) == (S ys) = (sort xs) == (sort ys)

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

disjoint :: (Set, Set) -> (Set, Set)
disjoint ((S xs), (S ys)) = if [y | y <- ys, not (elem y xs)] == ys then (S xs, S ys) else (S [], S [0])

sig =
  signature {
    maxTermSize = Just 8,
    instances = [
                 baseType (undefined::Set),
                 names (NamesFor ["s"] :: NamesFor Set)
                ],
    constants = [
       constant "empty" (empty :: Set),
       constant "union" (union :: Set -> Set -> Set),
       constant "singleton" (singleton :: Int -> Set),
       constant "size" (setSize :: Set -> Int),
       constant "+" ((+) :: Int -> Int -> Int),
       constant "fstdisjoints" (fst . disjoint),
       constant "snddisjoints" (snd . disjoint)
    ]
   }

main = quickSpec sig
