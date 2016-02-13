{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module APL where

import Data.Vector as V

class APLValue a where
    
    apl_eq :: a -> a -> Bool

instance APLValue Int where

    apl_eq = (==)

instance (Eq a, APLValue a) => APLValue (V.Vector a) where

    apl_eq = (==)

class Rohable a where
    
    roh_m :: a -> V.Vector Int

instance Rohable Int where
    
    roh_m = const V.empty

instance (Rohable a) => Rohable (V.Vector a) where
    
    roh_m v = (V.concatMap roh_m v) V.++ (V.singleton (V.length v))

class Tildeable a where
    
    tilde :: a -> a

instance Tildeable Int where

    tilde 0 = 1
    tilde 1 = 0
    tilde _ = error "Domain error"

instance Tildeable a => Tildeable (V.Vector a) where

    tilde = V.map tilde

class Equateable a where
    
    (<=>) :: a -> a -> a

instance Equateable Int where

    x <=> y = if x == y then
                1
              else
                0

instance (Equateable a) => Equateable (V.Vector a) where

    v <=> w
        | V.length v /= V.length w = error "Length error"
        | otherwise = V.zipWith (<=>) v w

iota_m :: Int -> V.Vector Int
iota_m 0 = V.empty
iota_m n 
    | n < 0 = error "Domain error"
    | otherwise = V.enumFromN 1 n

iota_d :: (APLValue a) => V.Vector a -> V.Vector a -> V.Vector Int
v `iota_d` w = V.map (iota_index v) w
    where
        iota_index :: (APLValue a) => V.Vector a -> a -> Int
        iota_index v a = case findIndex (apl_eq a) v of
                            Just i  -> i+1
                            Nothing -> (V.length v)+1

(</>) :: (a -> a -> a) -> V.Vector a -> a
(</>) = V.foldl1

(</\>) :: Int -> Int -> Int
(</\>) = (*)

(<\/>) :: Int -> Int -> Int
(<\/>) 0 0 = 0
(<\/>) _ _ = 1

ceiling_d :: Int -> Int -> Int
ceiling_d = max

floor_d :: Int -> Int -> Int
floor_d = min
