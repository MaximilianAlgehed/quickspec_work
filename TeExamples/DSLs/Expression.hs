{-# LANGUAGE FlexibleInstances #-}
module Expression where

import Control.Monad (guard)
import Test.QuickCheck


data Expression = Expression :+: Expression
                | Expression :-: Expression
                | Expression :*: Expression
                | Expression :/: Expression
                | V Int
                | X deriving (Eq, Ord)

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

exprAt :: Expression -> Expression -> Expression
exprAt (V d) v     = V d
exprAt X     v     = v
exprAt (e :+: f) v = (exprAt e v) :+: (exprAt f v)
exprAt (e :-: f) v = (exprAt e v) :-: (exprAt f v)
exprAt (e :*: f) v = (exprAt e v) :*:  (exprAt f v)
exprAt (e :/: f) v = (exprAt e v) :/: (exprAt f v)

derivative :: Expression -> Expression
derivative (V d)     = V 0
derivative X         = V 1
derivative (a :+: b) = simplify $ (derivative a) :+: (derivative b)
derivative (a :-: b) = simplify $ (derivative a) :-: (derivative b)
derivative (a :*: b) = simplify $ (a :*: (derivative b)) :+: ((derivative a) :*: b)
derivative (a :/: b) = simplify $ ((b :*: (derivative a)) :-: (a :*: (derivative b))) :/: (b :*: b)

evalFun :: Expression -> Int -> Maybe Int
evalFun (V d) x     = Just d
evalFun X x         = Just x
evalFun (a :+: b) x = (+) <$> evalFun a x <*> (evalFun b x)
evalFun (a :*: b) x = (*) <$> evalFun a x <*> (evalFun b x)
evalFun (a :-: b) x = (-) <$> evalFun a x <*> (evalFun b x)
evalFun (a :/: b) x = do
  a' <- evalFun a x
  b' <- evalFun b x
  guard (b' /= 0)
  return (a' `div` b')

showexp :: Expression -> String
showexp X         = "x"
showexp (V d)     = show d
showexp (a :+: b) = (showexp a) ++ " + " ++ (showexp b)
showexp (a :-: b) = (showexp a) ++ " - " ++ (showexp' b)
    where
        showexp' e@(_ :-: _) = paren (showexp e)
        showexp' e@(_ :+: _) = paren (showexp e)
        showexp' e           = showexp e
showexp (a :*: b) = (showexp' a) ++ " * " ++ (showexp' b)
    where
        showexp' e@(_ :-: _) = paren (showexp e)
        showexp' e@(_ :+: _) = paren (showexp e)
        showexp' e           = showexp e
showexp (a :/: b) = (showexp' a) ++ " / " ++ (showexp' b)
    where
        showexp' e@(_ :-: _) = paren (showexp e)
        showexp' e@(_ :+: _) = paren (showexp e)
        showexp' e@(_ :/: _) = paren (showexp e)
        showexp' e           = showexp e

paren :: String -> String
paren s = "("++s++")"

simplify :: Expression -> Expression
simplify X                     = X
simplify (V d)                 = V d
simplify (a :+: b)
    | isZero (simplify b)      = simplify a
    | isZero (simplify a)      = simplify b
    | otherwise                = (simplify a) :+: (simplify b)
simplify (a :-: b)
    | isZero (simplify b)      = a
    | otherwise                = (simplify a) :-: (simplify b)
simplify (a :*: b)
    | isZero (simplify a)      = V 0
    | isZero (simplify b)      = V 0
    | isOne (simplify a)       = (simplify b)
    | isOne (simplify b)       = (simplify a)
    | otherwise                = (simplify a) :*: (simplify b)
simplify (a :/: b)
    | isZero (simplify a)      = V 0
    | otherwise                = (simplify a) :/: (simplify b)

isOne :: Expression -> Bool
isOne X         = False
isOne (V d)     = d == 1
isOne (a :+: b) = ((isOne a) && (isZero b)) || ((isOne b) && (isZero a))
isOne (a :-: b) = (isZero b) && (isOne a)
isOne (a :*: b) = (isOne a) && (isOne b)
isOne (a :/: b) = (isOne a) && (isOne b)

isZero :: Expression -> Bool
isZero X         = False
isZero (V d)     = d == 0
isZero (a :+: b) = ((isZero a) && (isZero b)) || (a == b)
isZero (a :-: b) = (isOne a) && (isOne b)
isZero (a :*: b) = (isZero a) || (isZero b)
isZero (a :/: b) = isZero a

instance Show Expression where
    show = showexp
