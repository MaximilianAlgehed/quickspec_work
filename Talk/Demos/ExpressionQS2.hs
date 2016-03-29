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
import QuickSpec.PrintConditionally
import TemplateDerivingPredicates
import QuickSpec hiding (insert)
import Data.Coerce

pshow :: Expression -> Expression -> Bool
pshow v w = show v == show w

data Ppshow = Ps {a21 :: Expression, a22 :: Expression} deriving (Ord, Eq, Show)

mutate :: Expression -> Gen Expression
mutate e = transformM doMutate e
    where
        doMutate e@(a :+: (b :+: c)) = oneof [return e, return ((a :+: b) :+: c)]
        doMutate e                   = return e

instance Arbitrary Ppshow where
    
    arbitrary = arb `suchThat` (\p -> pshow (a21 p) (a22 p))
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
                    constant "v" a21,
                    constant "w" a22
                    ]
    }

main = do
         thy <- quickSpec sig
         putStrLn "==Laws=="
         printConditionally [(constant "eqShow" pshow, [constant "v" a21,
                                                        constant "w" a22])] thy
