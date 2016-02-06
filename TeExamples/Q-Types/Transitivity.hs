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

lt :: Int -> Int -> Bool
lt = (<)

$(mk_Predicates [
                 [| lt :: Int -> Int -> Bool |]
                ])

eq :: Plt -> Plt -> Bool
eq p1 p2 = (a22 p1) == (a21 p2)

$(mk_Predicates [
                [| eq :: Plt -> Plt -> Bool |]
               ])
sig =
  signature {
    maxTermSize = Just 12,
    instances = [
                 baseType (undefined::Plt),
                 names (NamesFor ["p'"] :: NamesFor Plt),
                 baseType (undefined::Peq),
                 names (NamesFor ["q'"] :: NamesFor Peq)
                ],
    constants = [
       constant "T" True,
       constant "F" False,
       constant "<" ((<) :: Int -> Int -> Bool),
       constant "x'" (coerce . a21 :: Plt -> Int),
       constant "y'" (coerce . a22 :: Plt -> Int),
       constant "p1'" (coerce . a21 :: Peq -> Plt),
       constant "p2'" (coerce . a22 :: Peq -> Plt)
    ]
   }

main = quickSpec sig
