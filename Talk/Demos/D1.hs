import Test.QuickCheck
import QuickSpec
import QuickSpec.PrintConditionally

data Peqlen = Peqlen {xs :: [Int], ys :: [Int]} deriving (Show, Eq, Ord)

instance Arbitrary Peqlen where
    arbitrary = do
                    l  <- arbitrary 
                    xs <- sequence $ replicate l arbitrary
                    ys <- sequence $ replicate l arbitrary
                    return (Peqlen xs ys)

constXS = constant "xs" (xs :: Peqlen -> [Int])
constYS = constant "ys" (ys :: Peqlen -> [Int])
constEqlen = constant "eqLength" ((\xs ys -> length xs == length ys) :: [Int] -> [Int] -> Bool)

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
        putStrLn "==Laws=="
        printConditionally [(constEqlen, [constXS, constYS])] thy       
