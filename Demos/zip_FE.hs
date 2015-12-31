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
        maxTermSize = Just 10,
        constants = [
                    constant "zip" (zip :: [Int] -> [Int] -> [(Int, Int)]),
                    constant "++" ((++) :: [Int] -> [Int] -> [Int]),
                    constant "length" (length :: [Int] -> Int),
                    constant "fst" (fst :: ([Int], [Int]) -> [Int]),
                    constant "snd" (snd :: ([Int], [Int]) -> [Int]),
                    constant "whenSameLength" (wheneqlen :: ([Int], [Int]) -> ([Int], [Int]))
                    ]
    }

main = quickSpec sig
