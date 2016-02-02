{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module A where
import qualified Text.Show.Functions
import qualified Data.Typeable as T
import qualified Prelude as P
import qualified Tip
data Nat = Z | S Nat deriving (P.Eq, P.Ord, P.Show, T.Typeable)
le :: Nat -> Nat -> P.Bool
le Z y = P.True
le (S z) Z = P.False
le (S z) (S x2) = le z x2
ge :: Nat -> Nat -> P.Bool
ge x Z = P.True
ge Z (S z2) = P.False
ge (S x22) (S z2) = ge x22 z2
equal :: Nat -> Nat -> P.Bool
equal Z Z = P.True
equal Z (S z3) = P.False
equal (S x23) Z = P.False
equal (S x23) (S y2) = equal x23 y2
prop x3 y3 = (ge x3 y3) Tip.==> ((le x3 y3) Tip.==> (equal x3 y3))
conj x4 y4 z4 = ((ge x4 y4) P.&& (ge y4 z4)) Tip.==> ((ge x4 z4))
