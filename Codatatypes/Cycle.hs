import qualified Data.List as L
import qualified Prelude as P

-- Lists that may be cyclic
data CList a = Nil | Cons a (CList a) | Cyc [a] deriving P.Show

-- Convert a cyclic list to a stream
toList :: CList a -> [a]
toList Nil          = []
toList (Cons a lst) = a:(toList lst)
toList (Cyc lst)    = L.cycle lst

-- Convert a list to a cyclic list
fromList :: [a] -> CList a
fromList []     = Nil
fromList (a:xs) = Cons a (fromList xs)

-- Append a cyclic list to another cyclic list
append :: CList a -> CList a -> CList a
append Nil as          = as
append (Cons a lst) as = Cons a (append lst as)
append as _            = as 

-- Repeat
repeat :: a -> CList a
repeat a = Cyc [a]

-- Reverse a Cyclic list
reverse :: CList a -> CList a
reverse Nil          = Nil
reverse (Cons a lst) = append (reverse lst) (Cons a Nil)
reverse (Cyc _)      = P.undefined

-- Take from a cyclic list
take :: P.Int -> CList a -> CList a
take i lst = fromList (P.take i (toList lst))

-- Drop from a cyclic list
drop :: P.Int -> CList a -> CList a
drop 0 lst          = lst
drop n Nil          = Nil
drop n (Cons a lst) = drop (n P.- 1) lst
drop n (Cyc lst)    = append hs (Cyc lst)
    where
        hs = fromList P.$ P.drop (n `P.mod` (P.length lst)) lst

-- Special toList function for equality
specialToList :: CList a -> ([a], [a])
specialToList = accToList ([], [])
    where
        accToList (xs, []) Nil          = (xs, [])
        accToList (xs, []) (Cons a lst) = accToList (a:xs, []) lst
        accToList (xs, []) (Cyc ys)     = (xs, ys)

-- Equality of pairs representing cyclic lists
pairEq :: (P.Eq a) => ([a], [a]) -> ([a], [a]) -> P.Bool
pairEq (xs, ys) (zs, []) = P.False
pairEq (xs, []) (ys, zs) = P.False
pairEq (xs, []) (ys, []) = xs P.== ys
pairEq (xs, ys) (zs, ws) = undefined -- To do

-- Equality over cyclic lists
instance (P.Eq a) => P.Eq (CList a) where
    (==) xs ys = pairEq (specialToList xs) (specialToList ys)
