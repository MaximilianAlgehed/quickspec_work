{-# LANGUAGE DeriveDataTypeable, TypeOperators, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import APL
import QuickSpec
import Test.QuickCheck
import Data.Vector as V
import TemplateDerivingPredicates
import Data.Coerce

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = fmap fromList arbitrary

newtype V = V (V.Vector Int) deriving (Ord, Eq, Show, Arbitrary)
newtype VV = VV (V.Vector (V.Vector Int)) deriving (Ord, Eq, Show, Arbitrary)

isBoolean :: Int -> Bool
isBoolean 0 = True
isBoolean 1 = True
isBoolean _ = False

isBooleanV :: V -> Bool
isBooleanV = V.all isBoolean . coerce

isBooleanV' :: VV -> Bool
isBooleanV' = V.all isBooleanV . coerce

$(mk_Predicates [[| isBoolean :: Int -> Bool |], [| isBooleanV :: V -> Bool |], [| isBooleanV' :: VV -> Bool |]])

sig =
  signature {
    maxTermSize = Just 7,
    instances = [
                 baseType (undefined::V.Vector Int),
                 names (NamesFor ["xs", "ys", "zs"] :: NamesFor (V.Vector Int)),
                 baseType (undefined::V.Vector (V.Vector Int)),
                 names (NamesFor ["xss", "yss", "zss"] :: NamesFor (V.Vector (V.Vector Int))),
                 baseType (undefined::PisBoolean),
                 names (NamesFor ["p"] :: NamesFor PisBoolean),
                 baseType (undefined::PisBooleanV),
                 names (NamesFor ["p"] :: NamesFor PisBooleanV),
                 baseType (undefined::PisBooleanV'),
                 names (NamesFor ["p"] :: NamesFor PisBooleanV')
                ],
    constants = [
        constant "x" (coerce . a11 :: PisBoolean -> Int),
        constant "xs" (coerce . a11 :: PisBooleanV -> V.Vector Int),
        constant "xss" (coerce . a11 :: PisBooleanV' -> V.Vector (V.Vector Int)),
        constant "⌈" (ceiling_d :: Int -> Int -> Int),
        constant "⌈" (ceiling_d :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "⌈" (ceiling_d :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        constant "⌊" (floor_d :: Int -> Int -> Int),
        constant "⌊" (floor_d :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "⌊" (floor_d :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        constant "⍳" (iota_m :: Int -> V.Vector Int),
        constant "⍳" (iota_m :: V.Vector Int -> V.Vector Int),
        constant "⍳" (iota_m :: V.Vector (V.Vector Int) -> V.Vector Int),
        constant "⍳" (iota_d :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "0" (0 :: Int),
        constant "1" (1 :: Int),
        constant "2" (2 :: Int),
        constant "⍴" (roh_m :: Int -> V.Vector Int),
        constant "⍴" (roh_m :: V.Vector Int -> V.Vector Int),
        constant "⍴" (roh_m :: V.Vector (V.Vector Int) -> V.Vector Int),
        constant "=" ((<=>) :: Int -> Int -> Int),
        constant "=" ((<=>) :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "=" ((<=>) :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        constant "∧" ((</\>) :: Int -> Int -> Int),
        constant "∧" ((</\>) :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "∧" ((</\>) :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        constant "∨" ((<\/>) :: Int -> Int -> Int),
        constant "∨" ((<\/>) :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "∨" ((<\/>) :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        constant "~" (tilde :: Int -> Int),
        constant "~" (tilde :: V.Vector Int -> V.Vector Int),
        constant "~" (tilde :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int))
    ]
   }

main = quickSpec sig
