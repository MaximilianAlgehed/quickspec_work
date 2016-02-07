{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Prelude hiding (lookup, insert)
import QuickSpec hiding (insert)
import Data.Coerce
import Test.QuickCheck
import TemplateDerivingPredicates

newtype QueueI = Q ([Integer],[Integer]) deriving (Arbitrary, Eq, Show, Ord)
emptyI = Q ([],[])
addI x (Q (f,b)) = flipQ (f,x:b)
isEmptyI (Q (f,b)) = null f
frontI (Q (x:f,b)) = x
removeI (Q (x:f,b)) = flipQ (f,b)

flipQ ([],b) = Q (reverse b,[])
flipQ q = Q q

retrieve :: QueueI -> [Integer]
retrieve (Q (f,b)) = f ++ reverse b

invariant :: QueueI -> Bool
invariant (Q (f,b)) = not (null f) || null b

add :: a -> [a] -> [a]
add x q = q ++ [x]

nisEmptyI :: QueueI -> Bool
nisEmptyI = not . isEmptyI

$(mk_Predicates [[| nisEmptyI :: QueueI -> Bool |],[| isEmptyI :: QueueI -> Bool |], [| invariant :: QueueI -> Bool |]])

sig =
    signature {
        maxTermSize = Just 10,
        instances = [
                    baseType (undefined::Pinvariant),
                    names (NamesFor ["p"] :: NamesFor Pinvariant),
                    baseType (undefined::PisEmptyI),
                    names (NamesFor ["q"] :: NamesFor PisEmptyI),
                    baseType (undefined::PnisEmptyI),
                    names (NamesFor ["nq"] :: NamesFor PnisEmptyI),
                    baseType (undefined::QueueI),
                    names (NamesFor ["Q"] :: NamesFor QueueI)
                    ],
        constants = [
                    constant "null" (null :: [A] -> Bool),
                    constant "invariant" invariant,
                    constant "isEmpty" isEmptyI,
                    constant "empty" emptyI, 
                    constant "add" addI,
                    constant "add'" (add :: A -> [A] -> [A]),
                    constant "front" frontI,
                    constant "remove" removeI,
                    constant "reitreve" retrieve,
                    constant ":" ((:) :: A -> [A] -> [A]),
                    constant "[]" ([] :: [A]),
                    constant "x" (coerce . a11 :: Pinvariant -> QueueI),
                    constant "x" (coerce . a11 :: PisEmptyI -> QueueI),
                    constant "x" (coerce . a11 :: PnisEmptyI -> QueueI)
                    ]
    }

main = quickSpec sig
