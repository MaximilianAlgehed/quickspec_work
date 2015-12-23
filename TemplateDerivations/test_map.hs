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

instance Arbitrary (Map Int Int) where
    arbitrary = fmap fromList arbitrary

sig =
  signature {
    maxTermSize = Just 10,
    instances = [
                 baseType (undefined::Map Int Int),
                 names (NamesFor ["m"] :: NamesFor (Map Int Int)),
                 baseType (undefined::Pneq),
                 names (NamesFor ["p"] :: NamesFor Pneq)
                ],
    constants = [
       constant "lookup" (lookup :: Int -> Map Int Int -> Maybe Int),
       constant "insert" (insert :: Int -> Int -> Map Int Int -> Map Int Int),
       constant "union" (union :: Map Int Int -> Map Int Int -> Map Int Int),
       constant "x" (coerce . a21 :: Pneq -> Int),
       constant "y"  (coerce . a22 :: Pneq -> Int)
    ]
   }

main = quickSpec sig
