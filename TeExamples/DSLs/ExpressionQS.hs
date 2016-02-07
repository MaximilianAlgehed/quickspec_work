{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Expression
import Test.QuickCheck
import TemplateDerivingPredicates
import QuickSpec hiding (insert)
import Data.Coerce

showAppJunk :: Expression -> Expression -> String -> String -> Bool
showAppJunk v w s t = show v ++ s == show w ++ t

$(mk_Predicates [[| showAppJunk :: Expression -> Expression -> String -> String -> Bool |]])

sig =
    signature {
        maxTermSize = Just 10,
        instances = [
                    baseType (undefined::PshowAppJunk),
                    names (NamesFor ["p"] :: NamesFor PshowAppJunk),
                    baseType (undefined::Expression),
                    names (NamesFor ["e", "f", "g"] :: NamesFor Expression)
                    ],
        constants = [
                    constant "null" (null :: [A] -> Bool),
                    constant "++" ((++) :: [A] -> [A] -> [A]),
                    constant ":" ((:) :: A -> [A] -> [A]),
                    constant "[]" ([] :: [A]),
                    constant "&&" (&&),
                    constant "v" (coerce . a41 :: PshowAppJunk -> Expression),
                    constant "w" (coerce . a42 :: PshowAppJunk -> Expression),
                    constant "s" (coerce . a43 :: PshowAppJunk -> String),
                    constant "t" (coerce . a44 :: PshowAppJunk -> String)
                    ]
    }

main = quickSpec sig
