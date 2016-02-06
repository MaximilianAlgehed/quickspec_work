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

neq :: Int -> Int -> Bool
neq = (/=)

$(mk_Predicates [
                 [| neq :: Int -> Int -> Bool |]
                ])

sig =
    signature {
        maxTermSize = Just 10,
        instances = [
                    baseType (undefined :: Double),
                    names (NamesFor ["i", "j", "k"] :: NamesFor Double),
                    baseType (undefined::Map Int Double), 
                    names (NamesFor ["m"] :: NamesFor (Map Int Double)),
                    baseType (undefined::Pneq),
                    names (NamesFor ["p"] :: NamesFor Pneq)
                    ],
        constants = [
                    constant "lookup" (lookup :: Int -> Map Int Double -> Maybe Double),
                    constant "insert" (insert :: Int -> Double -> Map Int Double -> Map Int Double),
                    constant "union" (union :: Map Int Double -> Map Int Double -> Map Int Double),
                    constant "x" (coerce . a21 :: Pneq -> Int),
                    constant "y"  (coerce . a22 :: Pneq -> Int)
                    ]
    }

main = quickSpec sig
