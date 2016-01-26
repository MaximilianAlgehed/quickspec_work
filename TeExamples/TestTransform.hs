{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import TemplateDerivingPredicates
import Test.QuickCheck
import QuickSpec
import Data.Coerce

newtype Lst = Lst [Int] deriving Arbitrary

gt :: Int -> Bool
gt x = x > 0

len :: Lst -> Int
len (Lst lst) = length lst

$(mk_Transforms [
                 ([| gt :: Int -> Bool |], [| len :: Lst -> Int |])
            ])
