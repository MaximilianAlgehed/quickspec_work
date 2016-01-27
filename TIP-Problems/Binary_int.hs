{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binary where
import Tip
import qualified Prelude as P

even :: P.Int -> P.Bool
even x = (x `P.mod` 2) P.== 0

bitZeroZero :: P.Int -> P.Bool
bitZeroZero b = even b

bit3Zero :: P.Int -> P.Bool
bit3Zero b = b P.<= 127

lsl :: P.Int -> P.Int
lsl x = P.mod (x P.* 2) 256

lsr :: P.Int -> P.Int
lsr x = P.div x 2

prop1 b = bitZeroZero b ==> (lsl (lsr b)) === b
prop2 b = bit3Zero b ==> (lsr (lsl b)) === b
