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

isPalindrome :: Lst -> Bool
isPalindrome xs = ((coerce xs) :: [Int]) == ((reverse (coerce xs)) :: [Int])

newtype Lst = Lst [Int] deriving (Show, Arbitrary, Ord, Eq)

mreverse :: Lst -> Lst
mreverse (Lst xs) = Lst (reverse xs)

$(mk_Transforms [([| isPalindrome :: Lst -> Bool |], [| mreverse :: Lst -> Lst |])])

sig =
  signature {
    maxTermSize = Just 12,
    instances = [baseType (undefined::TisPalindromemreverse),
                 names (NamesFor ["p"] :: NamesFor TisPalindromemreverse),
                 baseType (undefined::Lst),
                 names (NamesFor ["is", "js", "ks"] :: NamesFor Lst)
                ],
    constants = [
       constant "reverse" mreverse,
       constant "x"  xTisPalindromemreverse,
       constant "rx" xtTisPalindromemreverse
    ]
   }

main = quickSpec sig
