{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

-- 8 bit word
newtype Binary = B Int deriving (Ord, Eq, Show, Typeable)

instance Arbitrary Binary where
    arbitrary = return . B . (`mod` 256) =<< arbitrary

newtype BitSevenZero = BSZ {x::Binary} deriving (Ord, Eq, Show, Typeable)

instance Arbitrary BitSevenZero where
    arbitrary = return . BSZ . B . (`mod` 128) =<< arbitrary

newtype BitZeroZero = BZZ {y::Binary} deriving (Ord, Eq, Show, Typeable)

instance Arbitrary BitZeroZero where
    arbitrary = return . BZZ . B . (`mod` 256) . (*2) =<< arbitrary

lsl :: Binary -> Binary
lsl (B x) = B (x*2 `mod` 256)

lsr :: Binary -> Binary 
lsr (B x) = B (x `div` 2)

sig =
  signature {
    maxTermSize = Just 4,
    instances = [
                 baseType (undefined::Binary),
                 names (NamesFor ["b", "w"] :: NamesFor Binary),
                 baseType (undefined::BitSevenZero),
                 names (NamesFor ["p"] :: NamesFor BitSevenZero),
                 baseType (undefined::BitZeroZero),
                 names (NamesFor ["q"] :: NamesFor BitZeroZero)
                ],
    constants = [
       constant "lsl" lsl,
       constant "lsr" lsr,
       constant "x" (x :: BitSevenZero -> Binary),
       constant "y" (y :: BitZeroZero -> Binary)
    ]
   }

main = quickSpec sig
