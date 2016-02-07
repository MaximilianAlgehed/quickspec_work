{-# LANGUAGE StandaloneDeriving #-}
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

newtype Lst = Lst [Int] deriving (Show, Arbitrary, Ord, Eq)

ltl :: Int -> Lst -> Bool
ltl n (Lst lst) = (n > 0) && n < (length lst)

$(mk_Predicates [
                [| ltl :: Int -> Lst -> Bool |]
                ])

n = (coerce . a21 :: Pltl -> Int)
xs = (coerce . a22 :: Pltl -> [Int])

sig =
  signature {
    maxTermSize = Just 11,
    instances = [
                 baseType (undefined::Pltl),
                 names (NamesFor ["p"] :: NamesFor Pltl)
                ],
    constants = [
       constant "n" n,
       constant "xs" xs,
       constant "length" (length :: [A] -> Int),
       constant "last" (last :: [A] -> A),
       constant "drop" (drop :: Int -> [A] -> [A]),
       constant "++" ((++) :: [A] -> [A] -> [A]),
       constant "[]" ([] :: [A]),
       constant ":" ((:) :: A -> [A] -> [A])
    ]
   }

main = quickSpec sig
