import Test.QuickCheck

class Predicatable a where
    getPredicate :: a -> Bool

newtype Predicate a = P {x::a}

instance (Predicatable a, Arbitrary a) => Arbitrary (Predicate a) where
    arbitrary = return . P =<< arbitrary `suchThat` getPredicate

newtype IntWrapper = IntWrapper Int deriving (Show)

instance Arbitrary IntWrapper where
    arbitrary = return . IntWrapper =<< arbitrary

predicate :: IntWrapper -> Bool
predicate (IntWrapper x) = x > 0

instance Predicatable IntWrapper where
    getPredicate = predicate

instance (Show a) => Show (Predicate a) where
    show (P a) = "P "++(show a)

prop_test (P (IntWrapper x)) = x > 0
