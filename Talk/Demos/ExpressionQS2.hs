{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import DemoExpression
import Data.Generics.Uniplate.Data
import Data.Generics.SYB
import Test.QuickCheck
import TemplateDerivingPredicates
import QuickSpec hiding (insert)
import Data.Coerce

pshow :: Expression -> Expression -> Bool
pshow v w = show v == show w

data Ppshow = Ps {a41 :: Expression, a42 :: Expression} deriving (Ord, Eq, Show)

mutate :: Expression -> Gen Expression
mutate e = transformM doMutate e
    where
        doMutate e@(a :+: (b :+: c)) = oneof [return e, return ((a :+: b) :+: c)]
        doMutate e                   = return e

instance Arbitrary Ppshow where
    
    arbitrary = arb `suchThat` (\p -> pshow (a41 p) (a42 p))
                where
                    arb = do
                            e <- arbitrary
                            e' <- oneof $ [return e, mutate e, arbitrary]
                            return (Ps e e')

sig =
    signature {
        maxTermSize = Just 5,
        instances = [
                    baseType (undefined::Ppshow),
                    names (NamesFor ["p"] :: NamesFor Ppshow),
                    baseType (undefined::Expression),
                    names (NamesFor ["e", "f", "g"] :: NamesFor Expression)
                    ],
        constants = [
                    constant "v" (coerce . a41 :: Ppshow -> Expression),
                    constant "w" (coerce . a42 :: Ppshow -> Expression)
                    ]
    }

main = quickSpec sig
