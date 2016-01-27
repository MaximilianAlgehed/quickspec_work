{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Isort where 
import Tip
import qualified Prelude as P

data Nat = Z | S Nat deriving (P.Eq, P.Ord, P.Show)
gt :: Nat -> Nat -> P.Bool
gt x Z = P.True
gt Z (S z2) = P.False
gt (S x22) (S z2) = gt x22 z2

-- Insert
-- Precondition: arg2 is sorted
isert x [] = [x]
isert x (y:ys) 
    | gt x y = y:(isert x ys)
    | P.otherwise = x:y:ys

-- Insetion sort
isort [] = []
isort (x:xs) = isert x (isort xs)

sorted [] = P.True
sorted [x] = P.True
sorted (x:y:xs) = (gt y x) P.&& (sorted (y:xs))

lemma x is = (sorted is) ==> (sorted (isert x is))
prop1 is = sorted (isort is)
prop2 x y is = (gt x y) ==> ((isert x (y:is)) === (y:(isert x is)))
prop3 x y is = (gt x y) ==> ((isert y (x:is)) === (y:x:is))
