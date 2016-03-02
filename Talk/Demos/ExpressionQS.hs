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

showAppJunk :: Expression -> Expression -> String -> String -> Bool
showAppJunk v w s t = show v ++ s == show w ++ t

data PshowAppJunk = Ps {a41 :: Expression, a42 :: Expression, a43 :: String, a44 :: String} deriving (Ord, Eq, Show)

instance Arbitrary PshowAppJunk where
    
    arbitrary = arb `suchThat` (\p -> showAppJunk (a41 p) (a42 p) (a43 p) (a44 p))
                where
                    arb = do
                            e <- arbitrary
                            e' <- oneof $ [return e, arbitrary]
                            s <- arbitrary
                            s' <- oneof $ [return s, arbitrary]
                            return (Ps e e' s s')

sig =
    signature {
        maxTermSize = Just 5,
        instances = [
                    baseType (undefined::PshowAppJunk),
                    names (NamesFor ["p"] :: NamesFor PshowAppJunk),
                    baseType (undefined::Expression),
                    names (NamesFor ["e", "f", "g"] :: NamesFor Expression)
                    ],
        constants = [
                    constant "v" (coerce . a41 :: PshowAppJunk -> Expression),
                    constant "w" (coerce . a42 :: PshowAppJunk -> Expression),
                    constant "s" (coerce . a43 :: PshowAppJunk -> String),
                    constant "t" (coerce . a44 :: PshowAppJunk -> String)
                    ]
    }

main = quickSpec sig
