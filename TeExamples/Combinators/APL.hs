{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module APL where

import Data.Vector as V

class APLValue a where
    
    apl_eq :: a -> a -> Bool

instance APLValue Int where

    apl_eq = (==)

instance (Eq a, APLValue a) => APLValue (V.Vector a) where

    apl_eq = (==)

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

class Equateable a where
    
    (<=>) :: a -> a -> a

instance Equateable Int where

    x <=> y = if x == y then
                1
              else
                0

instance (Equateable a) => Equateable (V.Vector a) where

    (<=>) = V.zipWith (<=>)

class Andable a where
    
    (</\>) :: a -> a -> a

instance Andable Int where

    (</\>) = (*)

instance (Andable a) => Andable (V.Vector a) where

    (</\>) = V.zipWith (</\>)

class Orable a where

    (<\/>) :: a -> a -> a

instance Orable Int where

    (<\/>) 0 0 = 0  
    (<\/>) _ _ = 1

instance (Orable a) => Orable (V.Vector a) where

    (<\/>) = V.zipWith (<\/>)

class APLFoldable v a' a where

    (</>) :: (a -> a -> a) -> v -> a' 

instance APLFoldable (V.Vector Int) Int Int where

    (</>) = V.foldl1

instance (APLFoldable (V.Vector a') a' a) =>
         APLFoldable (V.Vector (V.Vector a')) (V.Vector a') a where

    (</>) fun = V.map ((</>) fun)

class DCeiling a where
    
    ceiling_d :: a -> a -> a

instance DCeiling Int where
    
    ceiling_d = max

instance (DCeiling a) => DCeiling (V.Vector a) where
    
    ceiling_d = V.zipWith ceiling_d

class DFloor a where
    
    floor_d :: a -> a -> a

instance DFloor Int where
    
    floor_d = min

instance (DFloor a) => DFloor (V.Vector a) where
    
    floor_d = V.zipWith floor_d

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
