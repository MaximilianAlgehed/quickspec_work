{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import TemplateDerivingPredicates
import Test.QuickCheck
import Data.Coerce
import Data.List hiding (union)
import QuickSpec hiding (S)

newtype Set = S {members::[Integer]} deriving (Ord, Typeable, Show)

instance Arbitrary Set where
    arbitrary = return . S . nub =<< arbitrary

instance Eq Set where
    (S xs) == (S ys) = (sort xs) == (sort ys)

disjoint :: Set -> Set -> Bool
disjoint (S s) s' = all (\x -> not (x `setMember` s')) s

setMember :: Integer -> Set -> Bool
setMember x (S xs) = elem x xs

$(mk_Predicates [[| disjoint :: Set -> Set -> Bool |]])

union :: Set -> Set -> Set
union (S xs) (S ys) = S $ sort $ nub (xs ++ ys)

singleton :: Integer -> Set
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
                 baseType (undefined::Pdisjoint),
                 names (NamesFor ["p"] :: NamesFor Pdisjoint),
                 baseType (undefined::Set),
                 names (NamesFor ["s"] :: NamesFor Set)
                ],
    constants = [
       constant "empty" (empty :: Set),
       constant "union" (union :: Set -> Set -> Set),
       constant "singleton" (singleton :: Integer -> Set),
       constant "x" (coerce . a21 :: Pdisjoint -> Set),
       constant "y" (coerce . a22 :: Pdisjoint -> Set),
       constant "size" (setSize :: Set -> Int),
       constant "+" ((+) :: Int -> Int -> Int),
       constant "1" (1 :: Int),
       constant "0" (0 :: Int)
    ]
   }

main = quickSpec sig
