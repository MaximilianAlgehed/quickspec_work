{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import DemoExpression
import Test.QuickCheck
import TemplateDerivingPredicates
import QuickSpec hiding (insert)
import QuickSpec.PrintConditionally
import Data.Coerce

pshow :: Expression -> Expression -> Bool
pshow v w = show v == show w

data Ppshow = Ps {a21 :: Expression, a22 :: Expression} deriving (Ord, Eq, Show)

instance Arbitrary Ppshow where
    
    arbitrary = arb `suchThat` (\p -> pshow (a21 p) (a22 p))
                where
                    arb = do
                            e <- arbitrary
                            e' <- oneof $ [return e, arbitrary]
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
