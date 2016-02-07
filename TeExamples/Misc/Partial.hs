{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import TemplateDerivingPredicates
import Test.QuickCheck
import QuickSpec
import Data.Coerce

gt :: Int -> Int -> Bool
gt = (>)

$(mk_Predicates [
                 [| gt :: Int -> Int -> Bool |]
                ])

partial :: Int -> Int -> Int
partial x y 
    | x > y = x

sig =
  signature {
    maxTermSize = Just 10,
    instances = [
                 baseType (undefined::Pgt),
                 names (NamesFor ["p"] :: NamesFor Pgt)
                ],
    constants = [
       constant "x" (coerce . a21 :: Pgt -> Int),
       constant "y" (coerce . a22 :: Pgt -> Int),
       constant "partial" (partial :: Int -> Int -> Int)
    ]
   }

main = quickSpec sig
