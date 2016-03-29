{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Prelude hiding ((^^), lookup, insert)
import QuickSpec hiding (insert)
import Data.Coerce
import Test.QuickCheck
import TemplateDerivingPredicates
import QuickSpec.PrintConditionally
import SetList

sig =
    signature {
        maxTermSize = Just 7,
        constants = [
                        constant "True" True,
                        constant "member" (member :: Integer -> [Integer] -> Bool),
                        constant "insert" (insert :: Integer -> [Integer] -> [Integer]),
                        constant "union" (union :: [Integer] -> [Integer] -> [Integer])
                    ]
    }

main = quickSpec sig
