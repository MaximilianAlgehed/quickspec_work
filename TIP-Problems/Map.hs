{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module A where
import qualified Text.Show.Functions
import qualified Data.Typeable as T
import qualified Prelude as P
import qualified Tip

data Maybe a = Just a | Nothing

isort :: [(P.Int, P.Int)] -> [(P.Int, P.Int)]
isort [] = []
isort (x:xs) = isert x (isort xs)

isert :: (P.Int, P.Int) -> [(P.Int, P.Int)] -> [(P.Int, P.Int)]
isert x [] = [x]
isert (x, y) ((z, w):is)
    | x P.> z = (z, w) : (isert (x, y) is)
    | P.otherwise = (x, y) : (z, w) : is

lookup :: P.Int -> [(P.Int, P.Int)] -> Maybe P.Int
lookup _ [] = Nothing
lookup a ((x, y):is) 
    | a P.== x = Just y
    | P.otherwise = Nothing

insert :: P.Int -> P.Int -> [(P.Int, P.Int)] -> [(P.Int, P.Int)]
insert a b [] = isort [(a, b)]
insert a b ((x, y):is)
    | a P.== x = isort ((a, b):is)
    | P.otherwise = isort ((x, y) : (insert a b is))

--conj :: (P.Ord a, P.Eq a) => a -> a -> b -> b -> [(a, b)] -> (P.Bool Tip.:=>: Tip.Equality [(a, b)])
conj x y i j m = (x P./= y) Tip.==> ((insert x i (insert y j m)) Tip.=== (insert y j (insert x i m)))
