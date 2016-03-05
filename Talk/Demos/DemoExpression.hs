{-# LANGUAGE DeriveDataTypeable,
             FlexibleInstances,
             GeneralizedNewtypeDeriving,
             DeriveAnyClass #-}

module DemoExpression where

import Control.Monad (guard)
import Test.QuickCheck
import Data.Data

data Expression = Expression :+: Expression
                | Expression :-: Expression
                | Expression :*: Expression
                | Expression :/: Expression
                | V Int
                | X deriving (Eq, Ord, Data, Typeable)

x = X

instance Arbitrary Expression where
    arbitrary = sized arb
        where
            arb :: Int -> Gen Expression
            arb 0 = oneof [fmap V arbitrary, return X]
            arb n = oneof [pure (:+:) <*> (arb n') <*> (arb n'),
                           pure (:*:) <*> (arb n') <*> (arb n'),
                           pure (:-:) <*> (arb n') <*> (arb n'),
                           pure (:/:) <*> (arb n') <*> (arb n'),
                           arb 0,
                           arb 0
                          ]
                          where
                            n' = n `div` 2

showexp :: Expression -> String
showexp X         = "x"
showexp (V d)     = show d
showexp (a :+: b) = (showexp a) ++ " + " ++ (showexp b)
showexp (a :-: b) = paren $ (showexp a) ++ " - " ++ (showexp b)
showexp (a :*: b) = paren $ (showexp a) ++ " * " ++ (showexp b)
showexp (a :/: b) = paren $ (showexp a) ++ " / " ++ (showexp b)

paren :: String -> String
paren s = "("++s++")"

instance Show Expression where
    show = showexp
