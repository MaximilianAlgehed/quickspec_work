{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

import TemplateDerivingPredicates
import Test.QuickCheck

$(mk_Predicate_Derivations 2)

instance Predicateable2 Int Int where
    predicate2 x y = x > y

prop_test :: Predicate2 Int Int -> Bool
prop_test p = (a1 p) > (a2 p)
