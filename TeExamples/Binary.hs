{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import TemplateDerivingPredicates
import Test.QuickCheck
import QuickSpec
import Data.Coerce

-- One byte
newtype Binary = B Int deriving (Ord, Eq, Show, Typeable)

instance Arbitrary Binary where
    arbitrary = return . B . (`mod` 256) =<< arbitrary

bitZeroZero :: Binary -> Bool
bitZeroZero (B b) = even b

bitSevenZero :: Binary -> Bool
bitSevenZero (B b) = b <= 127

$(mk_Predicates [
                 [| bitZeroZero :: Binary -> Bool |],
                 [| bitSevenZero :: Binary -> Bool |]
                ])

lsl :: Binary -> Binary
lsl (B x) = B (x*2 `mod` 256)

lsr :: Binary -> Binary 
lsr (B x) = B (x `div` 2)

sig =
  signature {
    maxTermSize = Just 4,
    instances = [
                 baseType (undefined::Binary),
                 names (NamesFor ["b"] :: NamesFor Binary),
                 baseType (undefined::PbitSevenZero),
                 names (NamesFor ["p"] :: NamesFor PbitSevenZero),
                 baseType (undefined::PbitZeroZero),
                 names (NamesFor ["q"] :: NamesFor PbitZeroZero)
                ],
    constants = [
       constant "lsl" lsl,
       constant "lsr" lsr,
       constant "x" (coerce . a11 :: PbitSevenZero -> Binary),
       constant "y" (coerce . a11 :: PbitZeroZero -> Binary)
    ]
   }

main = quickSpec sig
