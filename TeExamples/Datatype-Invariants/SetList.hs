{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module A where

import Prelude hiding ((^^), lookup, insert)
import QuickSpec hiding (insert)
import Data.Coerce
import Test.QuickCheck
import TemplateDerivingPredicates

disjointSets :: SetL -> SetL -> Bool
disjointSets a@(SetL s0) b@(SetL s1) = isSet a && isSet b && (and [not (elem x s1) | x <- s0])

isSet :: SetL -> Bool
isSet (SetL xs) = isSorted xs

member :: Int -> [Int] -> Bool
member _ [] = False
member a (x:is) 
    | a == x    = True 
    | a >  x    = False
    | a <  x    = True

insert :: Int -> [Int] -> [Int]
insert a [] = [a]
insert a (x:is)
    | a < x  = a:x:is
    | a == x = a:is
    | a > x  = x:(insert a is)

union :: [Int] -> [Int] -> [Int]
union [] xs     = xs
union xs []     = xs
union (x:xs) ys = insert x (union xs ys)     

newtype SetL = SetL [Int] deriving (Ord, Eq, Arbitrary)

$(mk_Predicates [[| isSet :: SetL -> Bool |], [| disjointSets :: SetL -> SetL -> Bool |]])

sig =
    signature {
        maxTermSize = Just 9,
        instances = [
                        baseType (undefined::PisSet),
                        names (NamesFor ["m", "n"] :: NamesFor PisSet),
                        baseType (undefined::PdisjointSets),
                        names (NamesFor ["q", "r"] :: NamesFor PdisjointSets)
                    ],
        constants = [
                        constant "member" (member :: Int -> [Int] -> Bool),
                        constant "insert" (insert :: Int -> [Int] -> [Int]),
                        constant "union" (union :: [Int] -> [Int] -> [Int]),
                        constant "x" (coerce . a11 :: PisSet -> [Int]),
                        constant "a" (coerce . a21 :: PdisjointSets -> [Int]),
                        constant "b" (coerce . a22 :: PdisjointSets -> [Int]),
                        constant "size" (length :: [A] -> Int),
                        constant "+" ((+) :: Int -> Int -> Int)
                    ]
    }

main = quickSpec sig
