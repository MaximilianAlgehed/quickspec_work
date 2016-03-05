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
import Data.Coerce

neq :: Expression -> Expression -> Bool
neq = (/=)

$(mk_Predicates [[| neq :: Expression -> Expression -> Bool |]])

sig =
    signature {
        maxTermSize = Just 5,
        instances = [
                    baseType (undefined::Pneq),
                    names (NamesFor ["p"] :: NamesFor Pneq),
                    baseType (undefined::Expression),
                    names (NamesFor ["e", "f", "g"] :: NamesFor Expression)
                    ],
        constants = [
                    constant "v" (coerce . a21 :: Pneq -> Expression),
                    constant "w" (coerce . a22 :: Pneq -> Expression),
                    constant "show" (show :: Expression -> String)
                    ]
    }

main = quickSpec sig
