{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Vector as V

class (Eq a) => APLValue a

class Rohable a where
    
    roh_m :: a -> V.Vector Int

instance APLValue Int
instance APLValue Char

instance (APLValue a) => Rohable a where
    
    roh_m = const V.empty

instance (APLValue a) => Rohable (V.Vector a) where
    
    roh_m = V.singleton . V.length

instance (APLValue a) => Rohable (V.Vector (V.Vector a)) where

    roh_m = V.map V.length


iota_m :: Int -> V.Vector Int
iota_m = V.enumFromN 1

iota_d :: (APLValue a) => V.Vector a -> V.Vector a -> V.Vector Int
v `iota_d` w = V.map (iota_index v) w
    where
        iota_index :: (APLValue a) => V.Vector a -> a -> Int
        iota_index v a = case findIndex (a==) v of
                            Just i  -> i+1
                            Nothing -> (V.length v)+1

(</>) :: (a -> a -> a) -> V.Vector a -> a
(</>) = V.foldl1

(</\>) :: Int -> Int -> Int
(</\>) = (*)

(<\/>) :: Int -> Int -> Int
(<\/>) x 0 = abs x
(<\/>) 0 x = abs x
(<\/>) 1 _ = 1
(<\/>) _ 1 = 1
