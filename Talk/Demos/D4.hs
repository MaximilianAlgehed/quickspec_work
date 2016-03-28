{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (lookup, insert)
import QuickSpec hiding (insert)
import QuickSpec.PrintConditionally
import Data.Coerce
import Test.QuickCheck
import TemplateDerivingPredicates
import DemoQueue

invariant :: Queue -> Bool
invariant (Q (f,b)) = not (null f) || null b

notEmpty :: Queue -> Bool
notEmpty = not . isEmpty

$(mk_Predicates [[| notEmpty :: Queue -> Bool |], [| invariant :: Queue -> Bool |]])

sig =
    signature {
        maxTermSize = Just 5,
        instances = [
                    baseType (undefined::Pinvariant),
                    names (NamesFor ["p"] :: NamesFor Pinvariant),
                    baseType (undefined::PnotEmpty),
                    names (NamesFor ["p"] :: NamesFor PnotEmpty),
                    baseType (undefined::Queue),
                    names (NamesFor ["q"] :: NamesFor Queue)
                    ],
        constants = [
                    constant "True" True,
                    constant "False" False,
                    constant "null" (null :: [A] -> Bool),
                    constant "invariant" invariant,
                    constant "notEmpty" notEmpty,
                    constant "isEmpty" isEmpty,
                    constant "empty" empty, 
                    constant "add" add,
                    constant "front" front,
                    constant "remove" remove,
                    constant "retrieve" retrieve,
                    constant ":" ((:) :: A -> [A] -> [A]),
                    constant "[]" ([] :: [A]),
                    constant "q" (coerce . a11 :: Pinvariant -> Queue),
                    constant "q" (coerce . a11 :: PnotEmpty -> Queue)
                    ]
    }

main = do
        thy <- quickSpec sig
        putStrLn "==Laws=="
        printConditionally [
            (constant "invariant" invariant, [constant "q" (coerce . a11 :: Pinvariant -> Queue)]),
            (constant "notEmpty" notEmpty, [constant "q" (coerce . a11 :: PnotEmpty -> Queue)])
            ] thy
