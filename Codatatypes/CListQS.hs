{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.QuickCheck
import QuickSpec
import Data.Coerce
import CyclicLists
import qualified Prelude as P

sig =
  signature {
    maxTermSize = P.Just 10,
    instances = [
                 baseType (P.undefined::(CList P.Integer)),
                 names (NamesFor ["xs", "ys", "zs"] :: NamesFor (CList P.Integer)),
                 baseType (P.undefined::(NC P.Integer)),
                 names (NamesFor ["p"] :: NamesFor (NC P.Integer))
                ],
    constants = [
       constant "[]" (nil :: CList P.Integer),
       constant ":" (cons :: P.Integer -> CList P.Integer -> CList P.Integer),
       constant "drop" (drop :: P.Int -> CList P.Integer -> CList P.Integer),
       constant "take" (take :: P.Int -> CList P.Integer -> CList P.Integer),
       constant "++" (append :: CList P.Integer -> CList P.Integer -> CList P.Integer),
       constant "xs" (coerce P.. P.id :: (NC P.Integer) -> CList P.Integer),
       constant "==" ((P.==) :: CList P.Integer -> CList P.Integer -> P.Bool),
       constant "True" P.True,
       constant "False" P.False
    ]
   }

main = quickSpec sig
