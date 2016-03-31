{-# LANGUAGE FlexibleInstances,UndecidableInstances,MultiParamTypeClasses,FlexibleContexts,ScopedTypeVariables #-}

module DemoAPL where

import Data.Vector as V
import Test.QuickCheck

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

instance (MRohable a, APLDyadic a) => APLDyadic (V.Vector a) where

    apl_zip f as bs 
        | (roh_m as) == (roh_m bs) = V.zipWith (apl_zip f) as bs
        | otherwise                = undefined

class APLMonadic a where
    
    apl_map :: (Int -> Int) -> a -> a

instance APLMonadic Int where
    
    apl_map f = f

instance (APLMonadic a) => APLMonadic (V.Vector a) where

    apl_map f = V.map (apl_map f)

class MRohable a where
    
    def :: a -> V.Vector Int
    roh_m :: a -> V.Vector Int

instance MRohable Int where
    
    def _ = V.empty
    roh_m = const V.empty

instance (MRohable a) => MRohable (V.Vector a) where
    
    def _ = (V.singleton 0) V.++ (def (undefined :: a))
    roh_m v 
        | V.length v == 0 = def v
        | otherwise       = (V.singleton (V.length v)) V.++
                            (V.concatMap roh_m (V.take 1 v))

class APLFoldable v a' a where

    (</>) :: (a -> a -> a) -> v -> a' 

instance APLFoldable (V.Vector Int) Int Int where

    (</>) = V.foldl1

instance (APLFoldable (V.Vector a') a' a) =>
         APLFoldable (V.Vector (V.Vector a')) (V.Vector a') a where

    (</>) fun = V.map ((</>) fun)

class Ravelable a where

    ravel :: a -> V.Vector Int

instance Ravelable Int where

    ravel = V.singleton
    
instance (Ravelable a) => Ravelable (V.Vector a) where
    
    ravel = V.concatMap ravel

class MIotable a where
    
    iota_m :: a -> V.Vector Int

instance MIotable Int where

    iota_m = iota
        where
            iota :: Int -> V.Vector Int
            iota 0          = V.empty
            iota n
                | n < 0     = undefined
                | otherwise = V.enumFromN 1 n

instance MIotable a => MIotable (V.Vector a) where

    iota_m = iota
        where
            iota v
             | V.length v == 1 = case v V.!? 0 of
                                    Nothing -> V.empty
                                    Just x  -> iota_m x
             | otherwise = undefined

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

cross :: APLDyadic a => a -> a -> a
cross = apl_zip apl_cross
    where
        apl_cross :: Int -> Int -> Int
        apl_cross = (*)

tilde :: APLMonadic a => a -> a
tilde = apl_map apl_tilde
    where
        apl_tilde 0 = 1
        apl_tilde 1 = 0
        apl_tilde _ = undefined

ceiling_d :: APLDyadic a => a -> a -> a
ceiling_d = apl_zip max

floor_d :: APLDyadic a => a -> a -> a
floor_d = apl_zip min

iota_d :: (APLValue a) => V.Vector a -> V.Vector a -> V.Vector Int
v `iota_d` w = V.map (iota_index v) w
    where
        iota_index :: (APLValue a) => V.Vector a -> a -> Int
        iota_index v a = case findIndex (apl_eq a) v of
                            Just i  -> i+1
                            Nothing -> (V.length v)+1

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbitrary
