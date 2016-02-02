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
data List a = Nil | Cons a (List a)
  deriving (P.Eq, P.Ord, P.Show, T.Typeable)
insert2 :: P.Int -> List P.Int -> List P.Int
insert2 x Nil = Cons x (Nil :: List P.Int)
insert2 x (Cons z xs) =
  case x P.<= z of
    P.True -> Cons x (Cons z xs)
    P.False -> Cons z (insert2 x xs)
isort :: List P.Int -> List P.Int
isort Nil = Nil :: List P.Int
isort (Cons y ys) = insert2 y (isort ys)
filter :: (b -> P.Bool) -> List b -> List b
filter x2 Nil = Nil :: List b
filter x2 (Cons z2 zs) =
  case P.id x2 z2 of
    P.True -> Cons z2 (filter x2 zs)
    P.False -> filter x2 zs
append :: List c -> List c -> List c
append Nil y2 = y2
append (Cons z3 xs2) y2 = Cons z3 (append xs2 y2)
qsort :: List P.Int -> List P.Int
qsort Nil = Nil :: List P.Int
qsort (Cons y3 xs3) =
  append
    (append
       (qsort (filter (\ z4 -> z4 P.<= y3) xs3))
       (Cons y3 (Nil :: List P.Int)))
    (qsort (filter (\ x22 -> x22 P.> y3) xs3))
sorted :: List P.Int -> P.Bool
sorted Nil = P.True
sorted (Cons x Nil) = P.True
sorted (Cons x (Cons y xs)) = (y P.>= x) P.&& (sorted (Cons y xs))

conj1 xs x = sorted xs Tip.=== (sorted (insert2 x xs))
conj xs = (sorted (qsort xs))
prop x3 = (qsort x3) Tip.=== (isort x3)
prop2 xs = sorted (qsort xs) Tip.=== sorted (qsort xs)
