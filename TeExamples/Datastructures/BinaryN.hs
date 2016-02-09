{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import TemplateDerivingPredicates
import Test.QuickCheck
import QuickSpec
import Data.Coerce

-- Bitvector
newtype Binary = B [Bool] deriving (Ord, Eq, Show)

fmap' :: ([Bool] -> [Bool]) -> Binary -> Binary
fmap' f (B b) = B (f b)

instance Arbitrary Binary where
    arbitrary = return . B =<< arbitrary `suchThat` (not . null)

bitsNZero :: Int -> Binary -> Bool
bitsNZero n (B bits) = (not . or) (take n bits)

$(mk_Predicates [
                 [| bitsNZero :: Int -> Binary -> Bool |]
                ])

lsl :: Int -> Binary -> Binary
lsl n (B x) = if n <= (length x) then B ((drop n x)++(take n (repeat False))) else B (take (length x) (repeat False))

lsr :: Int -> Binary -> Binary 
lsr n b = fmap' reverse $ lsl n $ fmap' reverse b

sig =
  signature {
    maxTermSize = Just 10,
    instances = [
                 baseType (undefined::Binary),
                 names (NamesFor ["b"] :: NamesFor Binary),
                 baseType (undefined::PbitsNZero),
                 names (NamesFor ["p"] :: NamesFor PbitsNZero)
                ],
    constants = [
       constant "lsl" lsl,
       constant "lsr" lsr,
       constant "b" (coerce . a22 :: PbitsNZero -> Binary),
       constant "n" (coerce . a21 :: PbitsNZero -> Int)
    ]
   }

main = quickSpec sig
