{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Max where 
import Tip
import qualified Prelude as P

data Nat = Z | S Nat deriving (P.Eq, P.Ord, P.Show)
gt :: Nat -> Nat -> P.Bool
gt x Z = P.True
gt Z (S z2) = P.False
gt (S x22) (S z2) = gt x22 z2

max :: Nat -> Nat -> Nat
max x y
    | gt x y = x
    | P.otherwise = y

prop i j k = max (max i j) k === max i (max j k)
lemma1 i j = max i j === max j i
lemma2 i j = gt i j ==> max i j === i
lemma3 i j k = gt i j ==> max (max i k) j === max i k
