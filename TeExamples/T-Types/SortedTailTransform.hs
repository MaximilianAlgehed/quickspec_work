{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import TemplateDerivingPredicates
import Test.QuickCheck
import QuickSpec
import Data.Coerce

newtype Lst = Lst [Int] deriving (Eq, Ord, Show)

instance Arbitrary Lst where
    arbitrary = fmap Lst $ arbitrary `suchThat` (not . null)

msorted :: Lst -> Bool
msorted (Lst xs) = isSorted xs

mtail :: Lst -> Lst
mtail (Lst xs) = Lst $ tail xs

-- Insert
-- Precondition: arg2 is sorted
isert :: Int -> Lst -> Lst
isert x (Lst []) = Lst [x]
isert x (Lst (y:ys))
    | x > y = Lst (y:(coerce (isert x (Lst ys))))
    | otherwise = Lst (x:y:ys)

-- Insetion sort
isort :: Lst -> Lst
isort (Lst []) = Lst []
isort (Lst (x:xs)) = isert x (isort (Lst xs))

mhead :: Lst -> Int
mhead (Lst xs) = head xs

$(mk_Transforms [([| msorted :: Lst -> Bool |], [| mtail :: Lst -> Lst |])])

sig =
  signature {
    maxTermSize = Just 10,
    instances = [baseType (undefined :: Tmsortedmtail),
                 names (NamesFor ["p"] :: NamesFor Tmsortedmtail),
                 baseType (undefined :: Lst),
                 names (NamesFor ["is", "js", "ks"] :: NamesFor Lst)
                ],
    constants = [
       constant "isort" isort, 
       constant "isert" isert, 
       constant "head" mhead,
       constant "sorted" msorted,
       constant "[]" ([] :: [A]),
       constant ":" ((:) :: A -> [A] -> [A]),
       constant "x" xTmsortedmtail,
       constant "xt" xtTmsortedmtail
    ]
   }

main = quickSpec sig

prop_head is = mhead (isert (mhead is) is) == mhead is
