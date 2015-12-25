{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (lookup)
import Test.QuickCheck
import QuickSpec hiding (insert)
import Data.Map
import Data.Coerce

whenDifferent :: (Int, Int) -> (Int, Int)
whenDifferent (x, y)
    | x /= y = (x, y)
    | otherwise = (0, 1)

instance Arbitrary (Map Int Int) where
    arbitrary = fmap fromList arbitrary

sig =
  signature {
    maxTermSize = Just 10,
    instances = [
                 baseType (undefined::Map Int Int),
                 names (NamesFor ["m"] :: NamesFor (Map Int Int))
                ],
    constants = [
       constant "lookup" (lookup :: Int -> Map Int Int -> Maybe Int),
       constant "insert" (insert :: Int -> Int -> Map Int Int -> Map Int Int),
       constant "union" (union :: Map Int Int -> Map Int Int -> Map Int Int),
       constant "whenDifferent" whenDifferent,
       constant "fst" (fst :: (Int, Int) -> Int),
       constant "snd" (snd :: (Int, Int) -> Int)
    ]
   }

main = quickSpec sig
