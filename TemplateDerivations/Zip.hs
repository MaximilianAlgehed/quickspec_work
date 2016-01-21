{-# LANGUAGE StandaloneDeriving #-}
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

-- Need wrapper for types-sake...
newtype Lst = Lst [Int] deriving (Typeable, Eq, Ord, Arbitrary, Show)

eqlen :: Lst -> Lst -> Bool
eqlen (Lst xs) (Lst ys) = (length xs) == (length ys)

$(mk_Predicates [
                 [| eqlen :: Lst -> Lst -> Bool |]
                ])

deriving instance Show TeqlenLst

sig =
    signature {
        maxTermSize = Just 10,
        instances = [
                    baseType (undefined::Peqlen),
                    names (NamesFor ["p"] :: NamesFor Peqlen)
                    ],
        constants = [
                    constant "zip" (zip :: [A] -> [B] -> [(A, B)]),
                    constant "++" ((++) :: [A] -> [A] -> [A]),
                    constant "length" (length :: [A] -> Int),
                    constant "Pxs" (coerce . a21 :: Peqlen -> [Int]),
                    constant "Pys"  (coerce . a22 :: Peqlen -> [Int]),
                    constant "reverse" (reverse :: [A] -> [A])
                    ]
    }

main = quickSpec sig

prop_pred :: Peqlen -> Bool
prop_pred p = zip (reverse (coerce (a21 p) :: [Int] )) (reverse (coerce (a22 p) :: [Int] )) == reverse (zip (coerce (a21 p) :: [Int]) (coerce (a22 p) :: [Int]))
