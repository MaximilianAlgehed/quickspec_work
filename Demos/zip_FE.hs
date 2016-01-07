{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (lookup)
import Test.QuickCheck
import QuickSpec hiding (insert)
import Data.Map
import Data.Coerce

wheneqlen :: ([Int], [Int]) -> ([Int], [Int])
wheneqlen (xs, ys)
    | (length xs) == (length ys) = (xs, ys)
    | otherwise = ([0], [1])

sig =
    signature {
        maxTermSize = Just 7,
        constants = [
                    constant "zip" (zip :: [Int] -> [Int] -> [(Int, Int)]),
                    constant "++" ((++) :: [Int] -> [Int] -> [Int]),
                    constant "fstwhenSameLength" ((fst . wheneqlen) :: ([Int], [Int]) -> [Int]),
                    constant "sndwhenSameLength" ((snd . wheneqlen) :: ([Int], [Int]) -> [Int])
                    ]
    }

main = quickSpec sig
