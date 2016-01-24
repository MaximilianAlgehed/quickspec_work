import Debug.Trace
import Data.Map hiding (map, filter, null)
import Data.List

transitivity_example = [Predicate "lt" 2, Predicate "lt" 2, Equality "lt" 2 "lt'" 1]
maximality_example = [Predicate "gt" 2, Predicate "gt" 2, Equality "gt" 1 "gt'" 1]
random_example = [Predicate "p1" 3, Predicate "p2" 2, Predicate "p3" 4, Equality "p1" 1 "p2" 1, Equality "p2" 2 "pr" 4]

compile preds = ordering
    where
        lst = fixNames preds
        connectivityList = makeConnectivityList $ renameDependencies $ getDependencies lst
        unappliedPredicates = getUnappliedPredicates $ renameDependencies $ getDependencies lst
        ordering = makeOrdering (map fst connectivityList) (unappliedPredicates)
        
        fixNames [] = []
        fixNames (e@(Equality _ _ _ _):xs) = e:(fixNames xs)
        fixNames ((Predicate s n):xs) = (Predicate s n):(fixNames (map (fixName s) xs))

        fixName name e@(Equality _ _ _ _) = e
        fixName name (Predicate s n) = Predicate (if s == name then s++"'" else s) n

type Arity = Int
type ArgumentNumber = Int
type PredicateName = String
type FunctionName = String
type ArgumentName = String
type ArgumentConnectivity = Int

data GeneratorOrdering = Arbitrary ArgumentName [PredicateApplication] deriving (Show)

data PredicateDefinition = Predicate PredicateName Arity
                         | Equality PredicateName ArgumentNumber PredicateName ArgumentNumber deriving (Show, Eq)

instance Ord PredicateDefinition where
    compare (Predicate _ _) (Equality _ _ _ _) = GT
    compare (Equality _ _ _ _) (Predicate _ _) = LT
    compare _ _ = EQ

data Dependency = PredicateDependent PredicateName [ArgumentName]
                | EqualityDependent ArgumentName ArgumentName deriving (Eq, Show)

--                                               Name          ArgumentNames  Numbers left to apply
data PredicateApplication = PredicateApplication PredicateName [ArgumentName] [ArgumentNumber] deriving (Show)

instance Ord Dependency where
    compare (EqualityDependent _ _) (PredicateDependent _ _) = LT
    compare (PredicateDependent _ _) (EqualityDependent _ _) = GT
    compare _ _ = EQ

makeOrdering [] _ = []
makeOrdering (x:xs) ps = (Arbitrary x appliedPreds):(makeOrdering xs preds')
    where
        preds = applyPredicates x ps
        preds' = filter notFullyApplied preds
        appliedPreds = filter (not . notFullyApplied) preds

        notFullyApplied (PredicateApplication _ _ xs) = not $ null xs

applyPredicates :: ArgumentName -> [PredicateApplication] -> [PredicateApplication]
applyPredicates name preds = map (applyArg name) preds
    where

        applyArg name (PredicateApplication pname args avail) = PredicateApplication pname args [indx | indx <- avail,
                                                                                                        (args !! (indx - 1)) /= name]
getUnappliedPredicates :: [Dependency] -> [PredicateApplication]
getUnappliedPredicates [] = []
getUnappliedPredicates ((PredicateDependent name arguments):xs) = (PredicateApplication name arguments [1..(length arguments)]):(getUnappliedPredicates xs)

getDependencies :: [PredicateDefinition] -> [Dependency]
getDependencies []                                      = [] 
getDependencies ((Equality name arity name' arity'):xs) = (EqualityDependent (name ++ (show arity)) (name' ++ (show arity'))):(getDependencies xs)
getDependencies ((Predicate name arity):xs)             = (PredicateDependent name [name++(show i) | i <- [1..arity]]):(getDependencies xs)

renameDependencies :: [Dependency] -> [Dependency]
renameDependencies = rename . sort
    where
        rename :: [Dependency] -> [Dependency]
        rename [] = []
        rename xs@((PredicateDependent _ _):_) = xs
        rename ((EqualityDependent s s'):xs) = rename (map (rename_arg s' s) xs)

        rename_arg :: String -> String -> Dependency -> Dependency
        rename_arg s s' (EqualityDependent s'' s''') = EqualityDependent (if s == s'' then s' else s'') (if s == s''' then s' else s''')
        rename_arg s s' (PredicateDependent name ss) = PredicateDependent name (map (\s_ -> if s == s_ then s' else s_) ss)

makeConnectivityList :: [Dependency] -> [(ArgumentName, ArgumentConnectivity)]
makeConnectivityList = reverse . (sortBy (\x y -> compare (snd x) (snd y))) . toList . (fromListWith (+)) . (concatMap flattenDependency)
    where
        flattenDependency :: Dependency -> [(ArgumentName, ArgumentConnectivity)]
        flattenDependency (PredicateDependent name args) = nub [(arg, length args) | arg <- args]
