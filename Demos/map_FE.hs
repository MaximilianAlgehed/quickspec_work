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
       constant "a" (fst . whenDifferent),
       constant "b" (snd . whenDifferent)
    ]
   }

main = quickSpec sig
