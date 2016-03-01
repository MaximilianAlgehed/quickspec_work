module BitstringDemo where
import Test.QuickCheck

-- Bitvector
newtype Bitstring = B [Bool] deriving (Ord, Eq, Show)

instance Arbitrary Bitstring where
    arbitrary = return . B =<< arbitrary `suchThat` (not . null)

map' :: ([Bool] -> [Bool]) -> Bitstring -> Bitstring
map' f (B b) = B (f b)

lsl :: Int -> Bitstring -> Bitstring
lsl n (B x)
    | n <= (length x) = B ((drop n x)++(take n (repeat False)))
    | otherwise       = B (take (length x) (repeat False))

lsr :: Int -> Bitstring -> Bitstring 
lsr n b = map' reverse $ lsl n $ map' reverse b
