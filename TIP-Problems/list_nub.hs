{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module A where
import QuickSpec hiding(S)
import Test.QuickCheck
import Prelude
import TemplateDerivingPredicates
import Data.Coerce
import qualified Text.Show.Functions
import qualified Tip

data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Show)

data Nat = Z | S Nat deriving (Eq, Ord, Show)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = sized fun
        where
            fun n = frequency [(1, return Nil), (n, Cons <$> arbitrary <*> fun (n-1))]

instance Arbitrary Nat where
    arbitrary = sized fun
        where
            fun n = frequency [(1, return Z), (n, S <$> fun (n-1) )]

equal :: Nat -> Nat -> Bool
equal Z Z = True
equal Z (S z) = False
equal (S x2) Z = False
equal (S x2) (S y2) = equal x2 y2
$(mk_Predicates [[| equal :: Nat -> Nat -> Bool |]])

deleteAll :: Nat -> List Nat -> List Nat
deleteAll x Nil = Nil :: List Nat
deleteAll x (Cons y xs) =
  case equal x y of
    True -> deleteAll x xs
    False -> Cons y (deleteAll x xs)
nub :: List Nat -> List Nat
nub Nil = Nil :: List Nat
nub (Cons y3 ys) = Cons y3 (deleteAll y3 (nub ys))
--prop zs = (nub (nub zs)) Tip.=== (nub zs)
sig =
  signature {
    maxTermSize = Just 10,
    instances = [
                 baseType (undefined::Nat),
                 names (NamesFor ["n", "m"] :: NamesFor Nat),
                 baseType (undefined::List Nat),
                 names (NamesFor ["is", "js", "ks"] :: NamesFor (List Nat)),
                 baseType (undefined::Pequal),
                 names (NamesFor ["p"] :: NamesFor (Pequal))
                ],
    constants = [
       constant "equal" equal,
       constant "deleteAll" deleteAll,
       constant "nub" nub,
       constant ":" (Cons :: Nat -> List Nat -> List Nat),
       constant "[]" (Nil :: List Nat),
       constant "S" S,
       constant "Z" Z,
       constant "x" (coerce . a21 :: Pequal -> Nat),
       constant "y" (coerce . a22 :: Pequal -> Nat)
    ]
   }

main = quickSpec sig
