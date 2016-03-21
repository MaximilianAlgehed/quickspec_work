{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DemoQueue where
import Test.QuickCheck

newtype Queue = Q ([Integer],[Integer]) deriving (Arbitrary, Eq, Show, Ord)

empty = Q ([],[])
add x (Q (f,b)) = flipQ (f,x:b)
isEmpty (Q (f,b)) = null f
front (Q (x:f,b)) = x
remove (Q (x:f,b)) = flipQ (f,b)

flipQ ([],b) = Q (reverse b,[])
flipQ q = Q q

retrieve :: Queue -> [Integer]
retrieve (Q (f,b)) = f ++ reverse b
