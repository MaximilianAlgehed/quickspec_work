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

$(mk_Predicates [[| invariant :: Queue -> Bool |]])

sig =
    signature {
        maxTermSize = Just 5,
        instances = [
                    baseType (undefined::Queue),
                    names (NamesFor ["q"] :: NamesFor Queue)
                    ],
        constants = [
                    constant "True" True,
                    constant "null" (null :: [A] -> Bool),
                    constant "invariant" invariant,
                    constant "isEmpty" isEmpty,
                    constant "notEmpty" notEmpty,
                    constant "empty" empty, 
                    constant "add" add,
                    constant "front" front,
                    constant "remove" remove,
                    constant "retrieve" retrieve,
                    constant ":" ((:) :: A -> [A] -> [A]),
                    constant "[]" ([] :: [A])
                    ]
    }

main = do
        thy <- quickSpec sig
        putStrLn "==Laws=="
        printConditionally [(constant "invariant" invariant, [constant "q" (coerce . a11 :: Pinvariant -> Queue)])] thy
