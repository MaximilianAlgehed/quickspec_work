import Data.Map hiding (map)
import Data.List

transitivity_example = [Predicate "lt1" 2, Predicate "lt2" 2, Equality "lt1" 2 "lt2" 1]

compile = makeConnectivityList . renameDependencies . getDependencies

type Arity = Int
type ArgumentNumber = Int
type PredicateName = String
type FunctionName = String
type ArgumentName = String
type ArgumentConnectivity = Int

data GeneratorOrdering = Finish [(PredicateName, [ArgumentName])]
                       | Arbitrary ArgumentName (Maybe PredicateName)
                       | Equate ArgumentName ArgumentName 
                       | ArbitrarySuchThat PredicateName [ArgumentName]
                       | PredicateConstruction PredicateName [ArgumentName] deriving (Show)

data PredicateDefinition = Predicate PredicateName Arity
                         | Equality PredicateName ArgumentNumber PredicateName ArgumentNumber deriving (Show)

data Dependency = PredicateDependent [ArgumentName]
                | EqualityDependent ArgumentName ArgumentName deriving (Eq, Show)

instance Ord Dependency where
    compare (PredicateDependent _) (PredicateDependent _) = EQ
    compare (EqualityDependent _ _) (EqualityDependent _ _) = EQ
    compare (EqualityDependent _ _) (PredicateDependent _) = LT
    compare (PredicateDependent _ ) (EqualityDependent _ _) = GT

getArgumentNames :: [PredicateDefinition] -> [ArgumentName]
getArgumentNames []                          = []
getArgumentNames ((Equality _ _ _ _):xs)     = getArgumentNames xs
getArgumentNames ((Predicate name arity):xs) = [name++(show i) | i <- [1..arity]] ++ (getArgumentNames xs)

getDependencies :: [PredicateDefinition] -> [Dependency]
getDependencies []                                      = [] 
getDependencies ((Equality name arity name' arity'):xs) = (EqualityDependent (name ++ (show arity)) (name' ++ (show arity'))):(getDependencies xs)
getDependencies ((Predicate name arity):xs)             = (PredicateDependent [name++(show i) | i <- [1..arity]]):(getDependencies xs)

renameDependencies :: [Dependency] -> [Dependency]
renameDependencies = rename . sort
    where
        rename :: [Dependency] -> [Dependency]
        rename [] = []
        rename xs@((PredicateDependent _):_) = xs
        rename ((EqualityDependent s s'):xs) = rename (map (rename_arg s' s) xs)

        rename_arg :: String -> String -> Dependency -> Dependency
        rename_arg s s' (EqualityDependent s'' s''') = EqualityDependent (if s == s'' then s' else s'') (if s == s''' then s' else s''')
        rename_arg s s' (PredicateDependent ss) = PredicateDependent (map (\s_ -> if s == s_ then s' else s_) ss)

makeConnectivityList :: [Dependency] -> [(ArgumentName, ArgumentConnectivity)]
makeConnectivityList = reverse . (sortBy (\x y -> compare (snd x) (snd y))) . toList . (fromListWith (+)) . (concatMap flattenDependency)
    where
        flattenDependency :: Dependency -> [(ArgumentName, ArgumentConnectivity)]
        flattenDependency (PredicateDependent args) = nub [(arg, length args) | arg <- args]
