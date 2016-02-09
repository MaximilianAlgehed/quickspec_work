{-# LANGUAGE DeriveDataTypeable, TypeOperators, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import TemplateDerivingPredicates
import Data.Coerce
import Control.Monad
import Test.QuickCheck
import QuickSpec hiding (render, Doc, background, (<>), text, nest, ($$))
import Text.PrettyPrint.HughesPJ
import qualified Text.PrettyPrint.HughesPJ as HUPJ

deriving instance Typeable Doc

instance Arbitrary Doc where
  arbitrary =
    sized $ \n ->
      let bin = resize (n `div` 2) arbitrary
          un = resize (n-1) arbitrary in
      oneof $
        [ liftM2 ($$) bin bin | n > 0 ] ++
        [ liftM2 (<>) bin bin | n > 0 ] ++
        [ liftM2 nest arbitrary un | n > 0 ] ++
        [ fmap text arbString ]

arbString :: Gen String
arbString = listOf (elements "ab")

background =
  signature {
    maxTermSize = Just 9,
    maxTests = Just 1000,
    constants = [
       constant "[]" ([] :: [A]),
       constant "++" ((++) :: [A] -> [A] -> [A]),
       constant "0" (0 :: Int),
       constant "+" ((+) :: Int -> Int -> Int),
       constant "length" (length :: String -> Int) ]}

-- obsDoc :: Doc -> Gen String
-- obsDoc d = do
--   n <- arbitrary
--   return (render (nest n d))

obsDoc :: Doc -> Gen String
obsDoc d = fmap render ctx
  where
    ctx =
      sized $ \n ->
      oneof $
        [ return d ] ++
        [ liftM2 op (resize (n `div` 2) ctx) (resize (n `div` 2) arbitrary) | n > 0, op <- [(<>), ($$)] ] ++
        [ liftM2 op (resize (n `div` 2) arbitrary) (resize (n `div` 2) ctx) | n > 0, op <- [(<>), ($$)] ] ++
        [ liftM2 nest arbitrary (resize (n-1) ctx) | n > 0 ]

unindented :: Doc -> Bool
unindented d = render (nest 100 (text "" <> d)) == render (nest 100 d)

nesting :: Doc -> Int
nesting d = head [ i | i <- nums, unindented (nest (-i) d) ]
  where
    nums = 0:concat [ [i, -i] | i <- [1..] ]

notNull :: String -> Bool
notNull = not . null

nested :: Doc -> Bool
nested = (>0) . nesting

$(mk_Predicates_No_Ord [[| unindented :: Doc -> Bool |], [| nested :: Doc -> Bool |]])
$(mk_Predicates [[| notNull :: String -> Bool |]])

sig =
  signature {
    maxTests = Just 1000,
    maxTermSize = Just 15,
    constants = [
       constant "P" (coerce . a11 :: Punindented -> Doc),
       constant "Q" (coerce . a11 :: Pnested -> Doc),
       constant "R" (coerce . a11 :: PnotNull -> String),
       constant "text" text,
       constant "nest" nest,
       --constant "nesting" nesting,
       constant "<>" (<>),
       constant "$$" ($$) ],
    instances = [
      inst (Sub Dict :: () :- Arbitrary Punindented),
      inst (Sub Dict :: () :- Arbitrary Pnested),
      baseType (undefined::PnotNull),
      names (NamesFor ["x"] :: NamesFor PnotNull),
      makeInstance (\() -> arbString),
      makeInstance (\() -> observe obsDoc),
      inst (Sub Dict :: () :- Arbitrary Doc) ],
    defaultTo = Just (typeOf (undefined :: Bool)) }

main = quickSpecWithBackground background sig
