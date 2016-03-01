{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import TemplateDerivingPredicates
import Test.QuickCheck
import BitstringDemo
import QuickSpec
import Data.Coerce

-- Predicate
bitsNZero :: Int -> Bitstring -> Bool
bitsNZero n (B bits) = (not . or) (take n bits)

-- Generate the type
$(mk_Predicates [[| bitsNZero :: Int -> Bitstring -> Bool |]])

sig =
    signature {
        maxTermSize = Just 8,
        instances = [
                     baseType (undefined::Bitstring),
                     names (NamesFor ["b"] :: NamesFor Bitstring),
                     baseType (undefined::PbitsNZero),
                     names (NamesFor ["p"] :: NamesFor PbitsNZero)
                    ],
        constants = [
                    constant "lsl" lsl,
                    constant "lsr" lsr,
                    constant "b" (coerce . a22 :: PbitsNZero -> Bitstring),
                    constant "n" (coerce . a21 :: PbitsNZero -> Int)
        ]
    }

main = quickSpec sig
