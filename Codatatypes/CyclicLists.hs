module CyclicLists where
import qualified Data.List as L
import qualified Prelude as P
import Test.QuickCheck

-- Lists that may be cyclic
data CList a = CL [a] [a] deriving (P.Show, P.Ord)

cons :: a -> CList a -> CList a
cons x (CL xs ys) = CL (x:xs) ys

nil :: CList a
nil = CL [] []

-- Convert a cyclic list to a stream
toList :: CList a -> [a]
toList (CL xs ys) = xs P.++ cycle ys

cycle [] = []
cycle xs = xs P.++ cycle xs

-- Convert a list to a cyclic list
fromList :: [a] -> CList a
fromList xs     = CL xs []

-- Append a cyclic list to another cyclic list
append :: CList a -> CList a -> CList a
append (CL xs []) (CL ys zs) = CL (xs P.++ ys) zs
append xs _                  = xs

-- Repeat
repeat :: a -> CList a
repeat a = CL [] (P.repeat a)

-- Reverse a Cyclic list
reverse :: CList a -> CList a
reverse (CL xs []) = CL (P.reverse xs) []
reverse _          = P.undefined

-- Take from a cyclic list
take :: P.Int -> CList a -> CList a
take i lst = fromList (P.take i (toList lst))

-- Drop from a cyclic list
drop :: P.Int -> CList a -> CList a
drop 0 lst            = lst
drop n (CL [] [])     = nil
drop n (CL [] ys)
    | n P.< 0     = (CL [] ys)
    | P.otherwise = CL xs ys
        where
            xs = P.drop (n `P.mod` (P.length ys)) ys
drop n (CL (x:xs) ys)
    | (n P.< 0)   = (CL (x:xs) ys)
    | P.otherwise = drop (n P.- 1) (CL xs ys)

-- Special toList function for equality
specialToList :: CList a -> ([a], [a])
specialToList (CL xs ys) = (xs, ys)

-- Equality of pairs representing cyclic lists
pairEq :: (P.Eq a) => ([a], [a]) -> ([a], [a]) -> P.Bool
pairEq (xs, (y:_)) (zs, []) = P.False
pairEq (xs, []) (ys, (z:_)) = P.False
pairEq (xs, []) (ys, [])    = xs P.== ys
pairEq (xs, ys) (zs, ws)
    | P.length xs P.== P.length zs = (xs P.== zs) P.&& (areInfEqual ys ws)
    | P.length xs P.>= P.length zs = (zs P.== (P.take (P.length zs) xs)) P.&&
                                     canRotateIn (P.drop (P.length zs) xs) ys P.&&
                                     areInfEqual (rotateIn (P.drop (P.length zs) xs) ys) ws
    | P.length xs P.<= P.length zs = (xs P.== (P.take (P.length xs) zs)) P.&&
                                     canRotateIn (P.drop (P.length xs) zs) ws P.&&
                                     areInfEqual (rotateIn (P.drop (P.length xs) zs) ws) ys

canRotateIn :: (P.Eq a) => [a] -> [a] -> P.Bool
canRotateIn xs ys = areInfEqual (rotate ys (P.length xs)) (rotateIn xs ys)

rotateIn :: [a] -> [a] -> [a]
rotateIn xs ys = rotateInHelper xs (P.concat (P.replicate (1 P.+ ((P.length xs) `P.div` (P.length ys))) ys))
    where
        rotateInHelper xs ys = P.take (P.length ys) (xs P.++ ys)

rotate :: [a] -> P.Int -> [a]
rotate xs 0 = xs
rotate xs n = rotate ([(L.last xs)] P.++ (P.init xs)) (n P.- 1)

-- Are two cycles directly equal
areInfEqual :: (P.Eq a) => [a] -> [a] -> P.Bool
areInfEqual xs ys
    | P.length xs P.== P.length ys = xs P.== ys
    | P.length xs P.>= P.length ys = ((P.take (P.length ys) xs) P.== ys) P.&&
                                     (areInfEqual (P.drop (P.length ys) xs) ys)
    | P.length ys P.>= P.length xs = ((P.take (P.length xs) ys) P.== xs) P.&&
                                     (areInfEqual (P.drop (P.length xs) ys) xs)

-- Equality over cyclic lists
instance (P.Eq a) => P.Eq (CList a) where
    (==) xs ys = pairEq (specialToList xs) (specialToList ys)

-- Arbitrary cyclic lists
instance (Arbitrary a) => Arbitrary (CList a) where
    arbitrary = do
        xs <- arbitrary
        ys <- arbitrary `suchThat` (P.not P.. P.null)
        P.return (CL xs ys)

-- Non-cyclic cyclic lists
newtype NC a = NC (CList a) deriving (P.Ord, P.Eq, P.Show)

-- Arbitrary non-cyclic lists are just lists
instance (Arbitrary a) => Arbitrary (NC a) where
    arbitrary = do
        xs <- arbitrary
        P.return (NC (CL xs []))

-- Some properties
prop_take_drop :: P.Int -> CList P.Int -> P.Bool
prop_take_drop i xs = (take i xs) `append` (drop i xs) P.== xs

prop_drop :: P.Int -> P.Int -> CList P.Int -> P.Bool
prop_drop i j xs = (drop i (drop j xs)) P.== (drop j (drop i xs))

prop_weird :: CList P.Int -> (NC P.Int) -> P.Int -> P.Bool
prop_weird xs (NC ys) i = P.not P.$ xs P.== (cons i ys)

prop_cyclic :: CList P.Int -> CList P.Int -> P.Int -> P.Bool
prop_cyclic xs ys i = ((cons i xs) P.== ys) P.== ((cons i ys) P.== xs)
