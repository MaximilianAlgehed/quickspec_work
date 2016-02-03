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

isMap :: [(P.Int, P.Int)] -> P.Bool
isMap [] = P.True
isMap ((x, y):xs) = (notelem x xs) P.&& (isMap xs)
    where
        notelem :: P.Int -> [(P.Int, P.Int)] -> P.Bool
        notelem v [] = P.True
        notelem v ((x, y):xs) = (P.not (v P.== x)) P.&& (notelem v xs)

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

conj x y i j m = isMap m Tip.==> (x P./= y) Tip.==> ((insert x i (insert y j m)) Tip.=== (insert y j (insert x i m)))
