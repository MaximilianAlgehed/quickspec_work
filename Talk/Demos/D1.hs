import Test.QuickCheck
import QuickSpec

-- Type to represent pairs of lists of equal length
data Peqlen = Peqlen {xs :: [Int], ys :: [Int]} deriving (Show, Eq, Ord)

-- This instance is completely arbitrary
instance Arbitrary Peqlen where
    arbitrary = do
                    -- Length of our lists
                    l  <- arbitrary 
                    
                    -- First list
                    xs <- sequence $ replicate l arbitrary
                    
                    -- Second list
                    ys <- sequence $ replicate l arbitrary

                    return (Peqlen xs ys)

-- QuickSpec signature
sig =
    signature {
        maxTermSize = Just 7,
        instances = [
                     baseType (undefined::Peqlen),
                     names (NamesFor ["p"] :: NamesFor Peqlen)
                    ],
        constants = [
                    constant "zip" (zip :: [Int] -> [Int] -> [(Int, Int)]),
                    constant "reverse" (reverse :: [A] -> [A]),
                    constant "xs" (xs :: Peqlen -> [Int]),
                    constant "ys" (ys :: Peqlen -> [Int]),
                    constant "++" ((++) :: [A] -> [A] -> [A])
                    ]
    }

main = quickSpec sig
