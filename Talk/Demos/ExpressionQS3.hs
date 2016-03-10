{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
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
        maxTermSize = Just 10,
        maxTests = Just 100000,
        instances = [
                    baseType (undefined::Pneq),
                    names (NamesFor ["p"] :: NamesFor Pneq),
                    baseType (undefined::Expression),
                    names (NamesFor ["e", "f", "g"] :: NamesFor Expression)
                    ],
        constants = [
                    constant "v" (coerce . a21 :: Pneq -> Expression),
                    constant "w" (coerce . a22 :: Pneq -> Expression),
                    constant "show" (show :: Expression -> String),
                    constant "/=" ((/=) :: String -> String -> Bool),
                    constant "True" True
                    ]
    }

deriving instance Show TneqExpression

prop :: Pneq -> Bool
prop p = show ((coerce . a21) p :: Expression) /= show ((coerce . a22) p :: Expression)

main = quickSpec sig
