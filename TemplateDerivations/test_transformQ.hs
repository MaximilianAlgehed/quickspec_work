{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Debug.Trace
import TemplateDerivingPredicates 
import QuickSpec
import Test.QuickCheck
import Data.Coerce

isPalindrome :: LInt -> Bool
isPalindrome xs = ((coerce xs) :: [Int]) == ((reverse (coerce xs)) :: [Int])

newtype LInt = LInt [Int] deriving (Typeable, Arbitrary, Ord, Eq)

$(mk_Predicates [[| isPalindrome :: LInt -> Bool |]])

isReverse :: LInt -> PisPalindrome -> Bool
isReverse xs pis = (coerce xs :: [Int]) == ((coerce . a11) pis :: [Int])

$(mk_Relations [[| isReverse :: LInt -> PisPalindrome -> Bool|]])

sig =
  signature {
    maxTermSize = Just 12,
    instances = [baseType (undefined::PisPalindrome),
                 names (NamesFor ["p"] :: NamesFor PisPalindrome),
                 baseType (undefined::PisReverse),
                 names (NamesFor ["q"] :: NamesFor PisReverse)
                ],
    constants = [
       constant "reverse" (reverse :: [Int] -> [Int]),
       constant "x" ((coerce . a11 . (coerce . a22 :: PisReverse -> PisPalindrome)) :: PisReverse -> [Int]),
       constant "rx" ((coerce . a21) :: PisReverse -> [Int])
       --constant "x" ((coerce . a11) :: PisPalindrome -> [Int])
    ]
   }

main = quickSpec sig
