{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module A where
import Prelude hiding (lookup, insert)
import QuickSpec hiding (insert)
import Data.Coerce
import Test.QuickCheck
import TemplateDerivingPredicates

isMap :: MapL -> Bool
isMap (MapL xs) = isMap' xs && isSorted (map fst xs)

isMap' :: [(Int, Double)] -> Bool
isMap' [] = True
isMap' ((x, y):xs) = (notelem x xs) && (isMap' xs)
    where
        notelem :: Int -> [(Int, Double)] -> Bool
        notelem v [] = True
        notelem v ((x, y):xs) = (not (v == x)) && (notelem v xs)

lookup :: Int -> [(Int, Double)] -> Maybe Double 
lookup _ [] = Nothing
lookup a ((x, y):is) 
    | a == x    = Just y 
    | otherwise = lookup a is

insert :: Int -> Double -> [(Int, Double)] -> [(Int, Double)]
insert a b [] = [(a, b)]
insert a b ((x, y):is)
    | a < x  = (a, b):(x, y):is
    | a == x = (a, b):is
    | a > x  = (x, y):(insert a b is)

union :: [(Int, Double)] -> [(Int, Double)] -> [(Int, Double)]
union [] xs     = xs
union xs []     = xs
union (x:xs) ys = union xs (insert (fst x) (snd x) ys)

newtype MapL = MapL [(Int, Double)] deriving (Ord, Eq, Arbitrary)

neq :: Int -> Int -> Bool
neq = (/=)

$(mk_Predicates [
                 [| neq :: Int -> Int -> Bool |],
                 [| isMap :: MapL -> Bool |]
                ])

sig =
    signature {
        maxTermSize = Just 10,
        instances = [
                    baseType (undefined :: Double),
                    names (NamesFor ["i", "j", "k"] :: NamesFor Double),
                    baseType (undefined::Pneq),
                    names (NamesFor ["p"] :: NamesFor Pneq),
                    baseType (undefined::PisMap),
                    names (NamesFor ["m", "n"] :: NamesFor PisMap)
                    ],
        constants = [
                    constant "lookup" (lookup :: Int -> [(Int, Double)] -> Maybe Double),
                    constant "insert" (insert :: Int -> Double -> [(Int, Double)] -> [(Int, Double)]),
                    constant "union" (union :: [(Int, Double)] -> [(Int, Double)] -> [(Int, Double)]),
                    constant "x" (coerce . a21 :: Pneq -> Int),
                    constant "y"  (coerce . a22 :: Pneq -> Int),
                    constant "x" (coerce . a11 :: PisMap -> [(Int, Double)])
                    ]
    }

main = quickSpec sig
