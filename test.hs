import Data.Typeable
import Data.Dynamic

type ArgumentTypes = [TypeRep] -- ?
type ReifiedArguments = [Dynamic] -- ? 
type Expression = (ArgumentTypes, ([Dynamic] -> Bool))

-- (P ==> Q) X == P(X) -> Q(X)
(==>) :: Expression -> Expression -> (ReifiedArguments, ReifiedArguments) -> Bool
(==>) (tps, f) (tps', f') 
    | [] == [x | x <- tps, x `elem` tps'] = (\ _ -> False)
    | otherwise = (\(dyn, dyn') -> case f dyn of
                                    False -> True
                                    True -> f' dyn')

-- Use rank 2 types maybe? To do some shenanigans on the type...
mkExpr :: (undefined -> Bool) -> Expression
mkExpr = undefined

implies :: (undefined -> Bool) -> (undefined -> Bool) -> (ReifiedArguments, ReifiedArguments) -> Bool
implies p q = (mkExpr p) ==> (mkExpr q)

-- Only needs to be tested against 2 values

-- How do we make sure that the predicate is _relevant_?
-- a relevant predicate uses at least one argument that is also used in the
-- expression, and if the predicate uses anything else it _only_ uses something that is
-- in normal form

-- Maybe we can use Data.Typeable for this, we just do all the work in the maybe monad.
-- This would imply that a test first checks if there is a type match in the arguments for the predicate
-- and the expression and then lifts the results from the maybe monad and compares them.

-- Question is how to compute an expression, what is allready there in terms of quickspec internals?
