{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

-- 8 bit word
newtype Binary = B Int deriving (Ord, Eq, Show, Typeable)

instance Arbitrary Binary where
    arbitrary = return . B . (`mod` 256) =<< arbitrary

lsl :: Binary -> Binary
lsl (B x) = B (x*2 `mod` 256)

lsr :: Binary -> Binary 
lsr (B x) = B (x `div` 2)

whenBitZeroZero :: Binary -> Binary
whenBitZeroZero (B x) = B (2*(x `div` 2))

whenBitSevenZero :: Binary -> Binary
whenBitSevenZero (B x) = B (x `mod` 128)

sig =
  signature {
    maxTermSize = Just 4,
    instances = [
                 baseType (undefined::Binary),
                 names (NamesFor ["b"] :: NamesFor Binary)
                ],
    constants = [
       constant "lsl" lsl,
       constant "lsr" lsr,
       constant "whenBitZeroZero" whenBitZeroZero,
       constant "whenBitSevenZero" whenBitSevenZero
    ]
   }

main = quickSpec sig
