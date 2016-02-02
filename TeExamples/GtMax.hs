{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding(max)
import TemplateDerivingPredicates
import Test.QuickCheck
import QuickSpec
import Data.Coerce

gt :: Int -> Int -> Bool
gt = (>)

max :: Int -> Int -> Int
max a b
    | a > b     = a
    | otherwise = b

$(mk_Predicates [
                [| gt :: Int -> Int -> Bool |]
                ])

sig =
  signature {
    maxTermSize = Just 10,
    instances = [
                 baseType (undefined::Pgt),
                 names (NamesFor ["p"] :: NamesFor Pgt)
                ],
    constants = [
       constant "max" max,
       constant "x" (coerce . a21 :: Pgt -> Int),
       constant "y" (coerce . a22 :: Pgt -> Int)
    ]
   }

main = quickSpec sig
