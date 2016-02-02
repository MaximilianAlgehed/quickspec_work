{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binary where
import Tip
import qualified Prelude as P

data Binary = B P.Bool P.Bool P.Bool deriving (P.Eq, P.Show)

bitZeroZero :: Binary -> P.Bool
bitZeroZero (B _ _ v) = P.not v

bit3Zero :: Binary -> P.Bool
bit3Zero (B v _ _) = P.not v

lsl :: Binary -> Binary
lsl (B _ a b) = B a b P.False

lsr :: Binary -> Binary
lsr (B a b _) = B P.False a b

prop1 b = bitZeroZero b ==> (lsl (lsr b)) === b
prop2 b = bit3Zero b ==> (lsr (lsl b)) === b
