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

data Token = A | B | C | D | ESC | P | Q | R deriving (Show, Eq, Ord)

instance Arbitrary Token where
    arbitrary = oneof (map return [A, B, C, D, ESC, P, Q, R])

escape :: [Token] -> [Token]
escape []                   = []
escape (x:xs) | isSpecial x = ESC : code x : escape xs
              | otherwise   = x : escape xs

isSpecial :: Token -> Bool
isSpecial ESC = True
isSpecial P   = True
isSpecial Q   = True
isSpecial R   = True
isSpecial _   = False

code :: Token -> Token
code ESC = ESC
code P   = A
code Q   = B
code R   = C
code x   = x

isEsc :: Token -> Bool
isEsc ESC = True
isEsc _   = False

ok :: Token -> Bool
ok x = not (isSpecial x) || isEsc x

$(mk_Predicates [[|isSpecial :: Token -> Bool|], [|isEsc :: Token -> Bool|], [|ok :: Token -> Bool|]])

sig =
  signature {
    maxTermSize = Just 10,
    instances = [
                 baseType (undefined::Token),
                 names (NamesFor ["t", "k", "j"] :: NamesFor Token),
                 baseType (undefined::Pok),
                 names (NamesFor ["pok"] :: NamesFor Pok),
                 baseType (undefined::PisEsc),
                 names (NamesFor ["pesc"] :: NamesFor PisEsc),
                 baseType (undefined::PisSpecial),
                 names (NamesFor ["pspec"] :: NamesFor PisSpecial)
                ],
    constants = [
       constant "x" (coerce . a11 :: Pok -> Token),
       constant "x" (coerce . a11 :: PisEsc -> Token),
       constant "x" (coerce . a11 :: PisSpecial -> Token),
       constant ":" ((:) :: Token -> [Token] -> [Token]),
       constant "[]" ([] :: [Token]),
       constant "++" ((++) :: [Token] -> [Token] -> [Token]),
       constant "code" code,
       constant "escape" escape
    ]
   }

main = quickSpec sig
