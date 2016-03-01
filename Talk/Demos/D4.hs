{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (lookup, insert)
import QuickSpec hiding (insert)
import DemoSets
import Data.Coerce
import Test.QuickCheck
import TemplateDerivingPredicates

newtype SetL = SetL [Int] deriving (Ord, Eq, Arbitrary)

$(mk_Predicates [[| isSet :: SetL -> Bool |], [| disjointSets :: SetL -> SetL -> Bool |]])

sig =
    signature {
        maxTermSize = Just 9,
        instances = [
                        baseType (undefined::PisSet),
                        names (NamesFor ["m", "n"] :: NamesFor PisSet),
                        baseType (undefined::PdisjointSets),
                        names (NamesFor ["q", "r"] :: NamesFor PdisjointSets)
                    ],
        constants = [
                        constant "member" (member :: Int -> [Int] -> Bool),
                        constant "insert" (insert :: Int -> [Int] -> [Int]),
                        constant "union" (union :: [Int] -> [Int] -> [Int]),
                        constant "x" (coerce . a11 :: PisSet -> [Int]),
                        constant "a" (coerce . a21 :: PdisjointSets -> [Int]),
                        constant "b" (coerce . a22 :: PdisjointSets -> [Int]),
                        constant "size" (length :: [A] -> Int),
                        constant "+" ((+) :: Int -> Int -> Int)
                    ]
    }

main = quickSpec sig
