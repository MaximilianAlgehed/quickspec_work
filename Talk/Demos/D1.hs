import Test.QuickCheck
import QuickSpec
import QuickSpec.PrintConditionally

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

constXS = constant "xs" (xs :: Peqlen -> [Int])
constYS = constant "ys" (ys :: Peqlen -> [Int])
constEqlen = constant "eqLength" ((\xs ys -> length xs == length ys) :: [Int] -> [Int] -> Bool)

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

main = do
        thy <- quickSpec sig
        printConditionally [(constEqlen, [constXS, constYS])] thy       
