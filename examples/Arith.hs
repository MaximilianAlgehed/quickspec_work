{-# LANGUAGE DeriveDataTypeable #-}
import QuickSpec
import Test.QuickCheck

data NotZero = NZ {x::Int} deriving (Show, Eq, Ord, Typeable)

instance Arbitrary NotZero where
    arbitrary = return . NZ =<< arbitrary `suchThat` (/=0)

sig =
  signature {
    maxTermSize = Just 7,
    instances = [
                 baseType (undefined::NotZero),
                 names (NamesFor ["p"] :: NamesFor NotZero)
                ], 
    constants = [
       constant "0" (0 :: Int),
       constant "1" (1 :: Int),
       constant "2" (2 :: Int),
       constant "x" (x :: NotZero -> Int),
       constant "-" ((\x -> -x) :: Int -> Int),
       constant "-" ((-) :: Int -> Int -> Int),
       constant "+" ((+) :: Int -> Int -> Int),
       constant "*" ((*) :: Int -> Int -> Int) ]}

main = quickSpec sig
