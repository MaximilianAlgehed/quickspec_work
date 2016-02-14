{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module APL where

import Data.Vector as V

class APLValue a where
    
    apl_eq :: a -> a -> Bool

instance APLValue Int where

    apl_eq = (==)

instance (Eq a, APLValue a) => APLValue (V.Vector a) where

    apl_eq = (==)

class APLDyadic a where

    apl_zip :: (Int -> Int -> Int) -> a -> a -> a

instance APLDyadic Int where
    
    apl_zip = ($)

instance (APLDyadic a) => APLDyadic (V.Vector a) where

    apl_zip f = V.zipWith (apl_zip f)

class MRohable a where
    
    roh_m :: a -> V.Vector Int

instance MRohable Int where
    
    roh_m = const V.empty

instance (MRohable a) => MRohable (V.Vector a) where
    
    roh_m v = (V.singleton (V.length v)) V.++
              (V.concatMap roh_m (V.take 1 v))

class Tildeable a where
    
    tilde :: a -> a

instance Tildeable Int where

    tilde 0 = 1
    tilde 1 = 0
    tilde _ = error "Domain error"

instance Tildeable a => Tildeable (V.Vector a) where

    tilde = V.map tilde

class APLFoldable v a' a where

    (</>) :: (a -> a -> a) -> v -> a' 

instance APLFoldable (V.Vector Int) Int Int where

    (</>) = V.foldl1

instance (APLFoldable (V.Vector a') a' a) =>
         APLFoldable (V.Vector (V.Vector a')) (V.Vector a') a where

    (</>) fun = V.map ((</>) fun)

(<=>) :: APLDyadic a => a -> a -> a
(<=>) = apl_zip apl_eq
    where
        apl_eq :: Int -> Int -> Int
        apl_eq x y = if x == y then
                        1
                     else
                        0

(</\>) :: APLDyadic a => a -> a -> a
(</\>) = apl_zip apl_and
    where
        apl_and :: Int -> Int -> Int
        apl_and x y = if x*y /= 0 then
                        1
                      else
                        0

(<\/>) :: APLDyadic a => a -> a -> a
(<\/>) = apl_zip apl_or
    where
        apl_or :: Int -> Int -> Int
        apl_or 0 0 = 0
        apl_or _ _ = 1

ceiling_d :: APLDyadic a => a -> a -> a
ceiling_d = apl_zip max

floor_d :: APLDyadic a => a -> a -> a
floor_d = apl_zip min

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
