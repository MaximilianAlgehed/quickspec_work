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
import DemoQueue

-- Predicates
invariant :: QueueI -> Bool
invariant (Q (f,b)) = not (null f) || null b

nisEmptyI :: QueueI -> Bool
nisEmptyI = not . isEmptyI

$(mk_Predicates [[| nisEmptyI :: QueueI -> Bool |], [| invariant :: QueueI -> Bool |]])

sig =
    signature {
        maxTermSize = Just 5,
        instances = [
                    baseType (undefined::Pinvariant),
                    names (NamesFor ["p"] :: NamesFor Pinvariant),
                    baseType (undefined::PnisEmptyI),
                    names (NamesFor ["q"] :: NamesFor PnisEmptyI),
                    baseType (undefined::QueueI),
                    names (NamesFor ["Q"] :: NamesFor QueueI)
                    ],
        constants = [
                    constant "null" (null :: [A] -> Bool),
                    constant "invariant" invariant,
                    constant "isEmpty" isEmptyI,
                    constant "empty" emptyI, 
                    constant "add" addI,
                    constant "front" frontI,
                    constant "remove" removeI,
                    constant "retrieve" retrieve,
                    constant ":" ((:) :: A -> [A] -> [A]),
                    constant "[]" ([] :: [A]),
                    constant "x" (coerce . a11 :: Pinvariant -> QueueI),
                    constant "x" (coerce . a11 :: PnisEmptyI -> QueueI)
                    ]
    }

main = quickSpec sig
