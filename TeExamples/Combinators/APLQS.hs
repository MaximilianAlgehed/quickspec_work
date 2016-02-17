{-# LANGUAGE DeriveDataTypeable, TypeOperators, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import APL
import Data.Function
import QuickSpec
import Test.QuickCheck
import qualified Data.Vector as V
import TemplateDerivingPredicates
import Data.Coerce

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbitrary

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

isWellShaped :: VV -> Bool
isWellShaped (VV v)
    | V.null v  = False
    | otherwise = V.all (\x -> if (V.length v) == 0 then True else x == (V.length (V.head v))) (V.map V.length v)

eqRohV :: V -> V -> Bool
eqRohV (V v) (V w) = (roh_m v) == (roh_m w)

erws :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> Bool
erws a b = (isWellShaped (VV a)) && (isWellShaped (VV b)) && ((roh_m a) == (roh_m b))

data Perws = Perws {p21 :: V.Vector (V.Vector Int), p22 :: V.Vector (V.Vector Int)} deriving (Eq, Ord, Show)

instance Arbitrary Perws where

    arbitrary = sized (\s ->
                        if s == 0 then
                            return (Perws V.empty V.empty)
                        else
                        do
                            n <- oneof (map return [1..s])
                            m <- oneof (map return [1..s])
                            a <- genVectorLen (genVectorLen arbitrary m) n
                            b <- genVectorLen (genVectorLen arbitrary m) n
                            return (Perws a b)
                )
                where
                    genVectorLen :: Gen a -> Int -> Gen (V.Vector a)
                    genVectorLen gen k = fmap V.fromList (sequence (replicate k gen))
                    

$(mk_Predicates [
                 --[| erws :: VV -> VV -> Bool |],
                 [| isWellShaped :: VV -> Bool |],
                 [| isBoolean :: Int -> Bool |],
                 [| isBooleanV :: V -> Bool |],
                 [| isBooleanV' :: VV -> Bool |],
                 [| eqRohV :: V -> V -> Bool |]
                ])

prop_erws :: Perws -> Bool
prop_erws e = erws (p21 e) (p22 e)

deriving instance Show TisWellShapedVV

prop_isw :: PisWellShaped -> PisWellShaped -> Bool 
prop_isw w w' = (roh_m (roh_m (coerce (a11 w) :: V.Vector (V.Vector Int)))) == (roh_m (roh_m (coerce (a11 w') :: V.Vector (V.Vector Int))))

sig =
  signature {
    maxTermSize = Just 10,
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
                 names (NamesFor ["p"] :: NamesFor PisBooleanV'),
                 baseType (undefined::PisWellShaped),
                 names (NamesFor ["q"] :: NamesFor PisWellShaped),
                 baseType (undefined::PeqRohV),
                 names (NamesFor ["w"] :: NamesFor PeqRohV),
                 baseType (undefined::Perws),
                 names (NamesFor ["e"] :: NamesFor Perws)
                ],
    constants = [
        --constant "x" (coerce . a11 :: PisBoolean -> Int),
        --constant "xs" (coerce . a11 :: PisBooleanV -> V.Vector Int),
        --constant "xs" (coerce . a21 :: PeqRohV -> V.Vector Int),
        --constant "ys" (coerce . a22 :: PeqRohV -> V.Vector Int),
        constant "xss" (coerce . a11 :: PisBooleanV' -> V.Vector (V.Vector Int)),
        constant "xss" (coerce . a11 :: PisWellShaped -> V.Vector (V.Vector Int)),
        constant "xss" (p21 :: Perws -> V.Vector (V.Vector Int)),
        constant "yss" (p22 :: Perws -> V.Vector (V.Vector Int)),
        --constant "⌈" (ceiling_d :: Int -> Int -> Int),
        --constant "⌈" (ceiling_d :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "⌈" (ceiling_d :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        --constant "⌊" (floor_d :: Int -> Int -> Int),
        --constant "⌊" (floor_d :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "⌊" (floor_d :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        --constant "⍳" (iota_m :: Int -> V.Vector Int),
        --constant "⍳" (iota_m :: V.Vector Int -> V.Vector Int),
        constant "⍳" (iota_m :: V.Vector (V.Vector Int) -> V.Vector Int),
        --constant "⍳" (iota_d :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "0" (0 :: Int),
        constant "1" (1 :: Int),
        constant "2" (2 :: Int),
        constant "⍴" (roh_m :: Int -> V.Vector Int),
        constant "⍴" (roh_m :: V.Vector Int -> V.Vector Int),
        constant "⍴" (roh_m :: V.Vector (V.Vector Int) -> V.Vector Int),
        --constant "==" ((<=>) :: Int -> Int -> Int),
        --constant "==" ((<=>) :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "==" ((<=>) :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        --constant "∧" ((</\>) :: Int -> Int -> Int),
        --constant "∧" ((</\>) :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "∧" ((</\>) :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        --constant "∨" ((<\/>) :: Int -> Int -> Int),
        --constant "∨" ((<\/>) :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "∨" ((<\/>) :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        --constant "~" (tilde :: Int -> Int),
        --constant "~" (tilde :: V.Vector Int -> V.Vector Int),
        constant "~" (tilde :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        constant "s" (V.singleton :: A -> V.Vector A)
    ]
   }

main = quickSpec sig
