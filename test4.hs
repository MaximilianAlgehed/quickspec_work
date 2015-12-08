{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import QuickSpec

data Greater = G {x::Int , y::Int} deriving (Eq, Ord, Typeable)
data MPos = NZ {z::Int} deriving (Eq, Ord, Typeable)
data SList = SL {ws::[Int]} deriving (Eq, Ord, Typeable)

instance Arbitrary Greater where
    arbitrary =
        do
            x <- arbitrary
            y <- arbitrary `suchThat` (x>)
            return (G x y)

instance Arbitrary MPos where
    arbitrary = return . NZ =<< arbitrary `suchThat` (>0)

instance Arbitrary SList where
    arbitrary = return . SL =<< arbitrary `suchThat` (isSorted)

sig =
  signature {
    maxTermSize = Just 9,
    instances = [
                 baseType (undefined::Greater),
                 names (NamesFor ["g"] :: NamesFor Greater),
                 baseType (undefined::MPos),
                 names (NamesFor ["pos"] :: NamesFor MPos),
                 baseType (undefined::SList),
                 names (NamesFor ["slist"] :: NamesFor SList)
                ],
    constants = [
       constant "True" (True :: Bool),
       constant "False" (False :: Bool),
       constant "==" ((==) :: Int -> Int -> Bool),
       constant "x" (x :: Greater -> Int),
       constant "y" (y :: Greater -> Int),
       constant "nil" ([] :: [Int]),
       constant "cons" ((:) :: Int -> [Int]->[Int]),
       constant "sorted" (isSorted :: [Int] -> Bool),
       constant "ws" (ws :: SList -> [Int])
       ]
   }

main = quickSpec sig
