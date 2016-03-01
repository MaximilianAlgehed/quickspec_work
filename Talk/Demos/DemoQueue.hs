{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DemoQueue where
import Test.QuickCheck

newtype QueueI = Q ([Integer],[Integer]) deriving (Arbitrary, Eq, Show, Ord)

emptyI = Q ([],[])
addI x (Q (f,b)) = flipQ (f,x:b)
isEmptyI (Q (f,b)) = null f
frontI (Q (x:f,b)) = x
removeI (Q (x:f,b)) = flipQ (f,b)

flipQ ([],b) = Q (reverse b,[])
flipQ q = Q q

retrieve :: QueueI -> [Integer]
retrieve (Q (f,b)) = f ++ reverse b

add :: a -> [a] -> [a]
add x q = q ++ [x]
