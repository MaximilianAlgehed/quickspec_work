{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck
import QuickSpec
import Data.Dynamic

fromJust (Just x) = x

class Predicateable a where
    toPredicates :: a -> Gen (Maybe [Dynamic]) -- This becomes really problematic
                                               -- when we have a lot of predicates
    getters      :: a -> [Int -> Constant]

instance Predicateable Bool where
    toPredicates True  = return (Just [])
    toPredicates False = return Nothing
    getters    = const []

instance (Predicateable b, Typeable a, Arbitrary a) => Predicateable (a -> b) where
    getters _ =
        (:)
        (\i ->
            constant
                ("acc"++show i)
                ((\(P preds) -> fromJust (fromDynamic (preds !! i))) :: Predicates -> a))
        (getters (undefined :: b))

    -- Here we need to do backtracking
    toPredicates predicate = do
                                a <- arbitrary
                                dyns <- toPredicates (predicate a)
                                case dyns of
                                    Nothing -> return Nothing
                                    Just xs -> return $ Just $ (toDyn a):xs

-- A type to hold _all_ predicates,
-- I imagine we will keep this type
-- hidden in QuickSpec
newtype Predicates = P [Dynamic]

-- Dummy instances, don't matter since we never inspect
-- the type (only it's projections)
instance Eq Predicates where
    p == q = False

instance Ord Predicates where
    compare p q = LT

-- Gathering the arbitrary predicates
-- means just generating one piece of data per input,
-- this get's more involved as we go to higher arity
-- predicates, but it should still be doable
instance Arbitrary Predicates where
    arbitrary = fmap (P . (map toDyn)) $ sequence [arbitrary `suchThat` ((not . null) :: [Int] -> Bool)]

sig =
  signature {
    maxTermSize = Just 10,
    -- here I imagine we will have a "predicates" field,
    -- from which we generate the arbitrary instance for the Predicates type
    instances = [
                 -- This type I imagine we will always add by default
                 baseType (undefined :: Predicates),
                 names (NamesFor ["p"] :: NamesFor Predicates)
                ],
    constants = [
       constant "[]" ([] :: [Int]),
       constant ":"  ((:) :: Int -> [Int] -> [Int]),
       constant "++" ((++) :: [Int] -> [Int] -> [Int]),
       constant "head" (head :: [Int] -> Int),
       -- I imagine we add one function like this for each argument
       constant "pxs" ((\(P preds) -> fromJust (fromDynamic (preds !! 0))) :: Predicates -> [Int])
    ]
   }

-- In short, I think the benefit of this idea is that
-- it should be possible to layer this on top of QuickSpec
-- with zero mucking about in the internals. All we need is just a "signature -> signature"
-- function that adds all these details, basically.

main = quickSpec sig
