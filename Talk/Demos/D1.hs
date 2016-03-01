{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.QuickCheck
import QuickSpec
import Data.Coerce
import Data.Function
import TemplateDerivingPredicates

newtype Lst = Lst [Int] deriving (Arbitrary, Ord, Eq, Show)

eqlen :: Lst -> Lst -> Bool
eqlen (Lst xs) (Lst ys) = on (==) length xs ys

$(mk_Predicates [[| eqlen :: Lst -> Lst -> Bool |]])

sig =
    signature {
        maxTermSize = Just 7,
        instances = [baseType (undefined::Peqlen),
                     names (NamesFor ["p"] :: NamesFor Peqlen)
                    ],
        constants = [
                    constant "zip" (zip :: [Int] -> [Int] -> [(Int, Int)]),
                    constant "length" (length :: [Int] -> Int),
                    constant "reverse" (reverse :: [Int] -> [Int]),
                    constant "xs" (coerce . a21 :: Peqlen -> [Int]),
                    constant "ys" (coerce . a22 :: Peqlen -> [Int]),
                    constant "++" ((++) :: [Int] -> [Int] -> [Int])
                    ]
    }

main = quickSpec sig
