{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Vector as V

class (Eq a) => APLValue a

instance APLValue Int

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
