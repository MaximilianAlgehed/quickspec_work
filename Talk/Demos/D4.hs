{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Prelude hiding (insert)
import QuickSpec hiding (insert)
import Data.Coerce
import Test.QuickCheck
import TemplateDerivingPredicates
import QuickSpec.PrintConditionally
import SetList

isSet :: SetL -> Bool
isSet (SetL xs) = isSorted xs

newtype SetL = SetL [Integer] deriving (Ord, Eq, Arbitrary, Show)

$(mk_Predicates [[| isSet :: SetL -> Bool |]])

sig =
    signature {
        maxTermSize = Just 7,
        instances = [
                        baseType (undefined::PisSet),
                        names (NamesFor ["m", "n"] :: NamesFor PisSet)
                    ],
        constants = [
                        constant "True" True,
                        constant "member" (member :: Integer -> [Integer] -> Bool),
                        constant "insert" (insert :: Integer -> [Integer] -> [Integer]),
                        constant "union" (union :: [Integer] -> [Integer] -> [Integer]),
                        constant "x" (coerce . a11 :: PisSet -> [Integer])
                    ]
    }

main = do
        thy <- quickSpec sig
        putStrLn "== Laws =="
        printConditionally [((constant "invariant" isSet), [constant "x" (coerce . a11 :: PisSet -> [Integer])])] thy

