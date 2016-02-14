import APL
import QuickSpec
import Test.QuickCheck
import Data.Vector as V

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = fmap fromList arbitrary

sig =
  signature {
    maxTermSize = Just 7,
    instances = [
                 baseType (undefined::V.Vector Int),
                 names (NamesFor ["xs", "ys", "zs"] :: NamesFor (V.Vector Int)),
                 baseType (undefined::V.Vector (V.Vector Int)),
                 names (NamesFor ["xss", "yss", "zss"] :: NamesFor (V.Vector Int))
                ],
    constants = [
        constant "⌈" (ceiling_d :: Int -> Int -> Int),
        constant "⌈" (ceiling_d :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "⌈" (ceiling_d :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        constant "⌊" (floor_d :: Int -> Int -> Int),
        constant "⌊" (floor_d :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "⌊" (floor_d :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)),
        constant "⍳" iota_m,
        constant "⍳" (iota_d :: V.Vector Int -> V.Vector Int -> V.Vector Int),
        constant "0" (0 :: Int),
        constant "1" (1 :: Int),
        constant "⍴" (roh_m :: Int -> V.Vector Int),
        constant "⍴" (roh_m :: V.Vector Int -> V.Vector Int),
        constant "⍴" (roh_m :: V.Vector (V.Vector Int) -> V.Vector Int)
    ]
   }

main = quickSpec sig
