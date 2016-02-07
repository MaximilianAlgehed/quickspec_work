{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (lookup)
import Test.QuickCheck
import QuickSpec hiding (insert)
import Data.Coerce
import Data.Function
import TemplateDerivingPredicates

newtype Lst = Lst [Int] deriving (Arbitrary, Ord, Eq, Show)

eqlen :: Lst -> Lst -> Bool
eqlen (Lst xs) (Lst ys) = on (==) length xs ys

$(mk_Predicates [[| eqlen :: Lst -> Lst -> Bool |]])

sig =
    signature {
        maxTermSize = Just 14,
        instances = [baseType (undefined::Peqlen),
                     names (NamesFor ["p"] :: NamesFor Peqlen)
                    ],
        constants = [
                    constant "zip" (zip :: [Int] -> [Int] -> [(Int, Int)]),
                    constant "length" (length :: [A] -> Int),
                    constant ":" ((:) :: A -> [A] -> [A]),
                    constant "[]" ([] :: [A]),
                    constant "reverse" (reverse :: [A] -> [A]),
                    constant "++" ((++) :: [Int] -> [Int] -> [Int]),
                    constant "xs" (coerce . a21 :: Peqlen -> [Int]),
                    constant "ys" (coerce . a22 :: Peqlen -> [Int]),
                    constant "++" ((++) :: [A] -> [A] -> [A])
                    ]
    }

main = quickSpec sig
