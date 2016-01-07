{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (lookup)
import TemplateDerivingPredicates
import Test.QuickCheck
import QuickSpec hiding (insert)
import Data.Map
import Data.Coerce

newtype Lst = Lst [Int] deriving (Typeable, Eq, Ord, Arbitrary)

eqlen :: Lst -> Lst -> Bool
eqlen (Lst xs) (Lst ys) = (length xs) == (length ys)

$(mk_Predicates [
                 [| eqlen :: Lst -> Lst -> Bool |]
                ])

sig =
    signature {
        maxTermSize = Just 7,
        instances = [
                    baseType (undefined::Peqlen),
                    names (NamesFor ["p"] :: NamesFor Peqlen)
                    ],
        constants = [
                    constant "zip" (zip :: [Int] -> [Int] -> [(Int, Int)]),
                    constant "++" ((++) :: [Int] -> [Int] -> [Int]),
                    --constant ":" ((:) :: Int -> [Int] -> [Int]),
                    --constant "[]" ([] :: [Int]),
                    constant "length" (length :: [Int] -> Int),
                    constant "xs" (coerce . a21 :: Peqlen -> [Int]),
                    constant "ys"  (coerce . a22 :: Peqlen -> [Int])
                    ]
    }

main = quickSpec sig
