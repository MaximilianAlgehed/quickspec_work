{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module APL where

import Data.Vector as V

class APLValue a where
    
    apl_eq :: a -> a -> Bool

instance APLValue Char where
    apl_eq = (==)

instance APLValue Int where
    apl_eq = (==)

class Rohable a where
    
    roh_m :: a -> V.Vector Int

instance Rohable Int where
    
    roh_m = const V.empty

instance Rohable Char where
    
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

iota_m :: Int -> V.Vector Int
iota_m = V.enumFromN 1

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
