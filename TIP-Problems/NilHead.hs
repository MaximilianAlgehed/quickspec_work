{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module A where

import Tip
import qualified Prelude as P

data Maybe a = Just a | Nothing deriving (P.Eq)

head :: [a] -> Maybe a
head [] = Nothing
head (x:xs) = Just x

notNill :: [a] -> P.Bool
notNill [] = P.False
notNill xs = P.True

(++) :: [a] -> [a] -> [a]
[] ++ a = a
(x:xs) ++ ys = x:(xs ++ ys)

isJust (Just xs) = P.True
isJust Nothing = P.False

conj xs ys = head (xs ++ ys) === head xs
conj1 xs ys = notNill (xs ++ ys) ==> (notNill xs) P.|| (notNill ys)
conj2 xs = notNill xs === (isJust (head xs))
conj4 xs ys = notNill xs ==> head (xs ++ ys) === head xs
