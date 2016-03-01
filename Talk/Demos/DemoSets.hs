{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DemoSets where
import QuickSpec hiding (insert)
import Test.QuickCheck

newtype SetL = SetL [Int] deriving (Ord, Eq, Arbitrary)

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
