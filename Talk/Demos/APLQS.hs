import DemoAPL
import QuickSpec
import Test.QuickCheck
import qualified Data.Vector as V
import QuickSpec.PrintConditionally

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbitrary

type VV = V.Vector (V.Vector Int) 

isWellShaped :: VV -> Bool
isWellShaped v
    | V.null v  = True
    | otherwise =
        V.all (\x -> x == (V.length (V.head v))
              ) (V.map V.length v)

erws :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> Bool
erws a b = (isWellShaped a) && (isWellShaped b) && ((roh_m a) == (roh_m b))

data Perws = Perws {p21 :: V.Vector (V.Vector Int), p22 :: V.Vector (V.Vector Int)} deriving (Eq, Ord, Show)

instance Arbitrary Perws where

    arbitrary = sized (\s ->
                        if s == 0 then
                            return (Perws V.empty V.empty)
                        else
                        do
                            n <- oneof (map return [1..s])
                            m <- oneof (map return [1..s])
                            a <- genVectorLen (genVectorLen arbitrary m) n
                            b <- genVectorLen (genVectorLen arbitrary m) n
                            return (Perws a b)
                )
                where
                    genVectorLen :: Gen a -> Int -> Gen (V.Vector a)
                    genVectorLen gen k = fmap V.fromList (sequence (replicate k gen))
                    
sig =
  signature {
    maxTermSize = Just 7,
    instances = [
                 baseType (undefined::V.Vector Int),
                 names (NamesFor ["xs", "ys", "zs"] :: NamesFor (V.Vector Int)),
                 baseType (undefined::VV),
                 names (NamesFor ["xss", "yss", "zss"] :: NamesFor VV),
                 baseType (undefined::Perws),
                 names (NamesFor ["e"] :: NamesFor Perws)
                ],
    constants = [
        constant "xss" (p21 :: Perws -> V.Vector (V.Vector Int)),
        constant "yss" (p22 :: Perws -> V.Vector (V.Vector Int)),
        constant "⍴" (roh_m :: VV -> V.Vector Int),
        constant "×" (cross :: VV -> VV -> VV)
    ]
   }

main = do
        thy <- quickSpec sig
        putStrLn "== Laws =="
        printConditionally [(constant "wellBehaved" erws, [
            constant "xss" (p21 :: Perws -> VV),
            constant "yss" (p22 :: Perws -> VV)])] thy
