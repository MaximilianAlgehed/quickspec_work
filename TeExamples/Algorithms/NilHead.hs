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

newtype Lst = Lst [Int] deriving (Ord, Eq, Typeable, Arbitrary)

notNill :: Lst -> Bool
notNill = not . null . (coerce :: Lst -> [Int])

isNill :: Lst -> Bool
isNill = not . notNill

$(mk_Predicates [
                 [| notNill :: Lst -> Bool |],
                 [| isNill :: Lst -> Bool |]
                ])

sig =
  signature {
    maxTermSize = Just 13,
    instances = [
                 baseType (undefined::PnotNill),
                 names (NamesFor ["ps"] :: NamesFor PnotNill),
                 baseType (undefined::PisNill),
                 names (NamesFor ["qs"] :: NamesFor PisNill),
                 baseType (undefined::Lst),
                 names (NamesFor ["is", "js", "ks"] :: NamesFor Lst)
                ],
    constants = [
       constant "[]" (Lst [] :: Lst),
       constant ":" ((\x xs -> Lst (x:(coerce xs))) :: Int -> Lst -> Lst),
       constant "xs" (coerce . a11 :: PnotNill -> Lst),
       constant "ys" (coerce . a11 :: PisNill -> Lst),
       constant "++" ((\xs ys -> Lst ((coerce xs) ++ (coerce ys))) :: Lst -> Lst -> Lst),
       constant "head" (head . coerce :: Lst -> Int)
    ]
   }

main = quickSpec sig
