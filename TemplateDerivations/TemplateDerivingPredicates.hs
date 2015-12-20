{-# LANGUAGE TemplateHaskell #-}

module TemplateDerivingPredicates where

import Language.Haskell.TH
import Data.Typeable

-- Creates all the plumbing from the function name and signature
-- example mk_Predicates [[| example :: Int -> Int -> Bool |]]
-- will generate the following:
--
-- type TExample = Predicate2 IntExample IntExample
-- newtype IntExample = IntExample Int
--
-- including all the plumbing in the background (all the predicate type instances etc)
mk_Predicates :: [ExpQ] -> Q [Dec]
mk_Predicates = undefined

-- Creates the entire predicate derivation structure when spliced
mk_Predicate_Types :: Integer -> Q [Dec]
mk_Predicate_Types n =
     return [mk_Predicate_N n,
             mk_Predicateable_N n,
             mk_Arbitrary_Instance_Predicate_N n
            ]

-- Create names [x1, x2, ..., xn]
make_names :: Integer -> [Name]
make_names n = [mkName ("x"++(show j)) | j <- [1..n]]

-- Creates function application typesignature for a predicate of arity n
mk_Typesig_N :: Integer -> Type 
mk_Typesig_N n = applications n
    where
        applications 0 = ConT (mkName "Bool")
        applications k = AppT (AppT ArrowT (VarT (mkName ("x"++(show (n-k+1)))))) (applications (k-1))

-- Creates a record with n fields
mk_Record_N :: Integer -> Con
mk_Record_N n = RecC name fields
    where
        name = (mkName ("P"++(show n)))
        fields = [(mkName ("a"++(show n)++(show j)), NotStrict, VarT (mkName ("x"++(show j)))) | j <- [1..n]]

-- Creates a predicate datatype with a record
mk_Predicate_N :: Integer -> Dec
mk_Predicate_N n = DataD [] name type_arguments [mk_Record_N n] derive_instances
    where
        derive_instances = [''Ord, ''Eq, ''Typeable, ''Show]
        name = (mkName ("Predicate"++(show n)))
        type_arguments = map PlainTV (make_names n)

-- Creates a predicateable class declaration
mk_Predicateable_N :: Integer -> Dec 
mk_Predicateable_N n = ClassD [] name type_arguments [] [predicate] 
    where
        name = mkName ("Predicateable"++(show n))
        predicate = SigD (mkName ("predicate"++(show n))) (mk_Typesig_N n)
        type_arguments = map PlainTV (make_names n)

-- generates the definiton unwrapn (x1, x2, ... , xn) = predicaten x1 x2 ... xn)
mk_Touple_To_Predicate_Function_N :: Integer -> Dec
mk_Touple_To_Predicate_Function_N n = FunD (mkName ("unwrap"++(show n))) [clause]
    where
        clause = Clause pattern body []
        pattern = [TupP (map VarP (make_names n))]
        body = NormalB (applications n)
        applications 0 = VarE (mkName ("predicate"++(show n)))
        applications k = AppE (applications (k-1)) (VarE (mkName ("x"++(show k))))

-- generates the expression " arbitrary `suchThat` unwrapn "
mk_Arbitrary_Function_Call_N :: Integer -> Exp 
mk_Arbitrary_Function_Call_N n = (AppE
                                    (AppE
                                        (VarE (mkName "suchThat"))
                                        (VarE (mkName "arbitrary"))
                                    ) 
                                    (VarE (mkName ("unwrap"++(show n))))
                                 )

-- generates the arbitrary = do ... call
mk_Arbitrary_Declaration_N :: Integer -> Dec
mk_Arbitrary_Declaration_N n = FunD (mkName "arbitrary") [clause]
    where
        clause = Clause [] body [mk_Touple_To_Predicate_Function_N n]
        body = NormalB (DoE [bind, ret])
        bind = BindS (TupP (map VarP (make_names n))) (mk_Arbitrary_Function_Call_N n)
        ret = NoBindS (AppE (VarE (mkName "return")) (applications n))
        applications 0 = ConE (mkName ("P"++(show n)))
        applications k = AppE (applications (k-1)) (VarE (mkName ("x"++(show k))))

-- Creates an instance of arbitrary for an arity n predicate type
mk_Arbitrary_Instance_Predicate_N :: Integer -> Dec
mk_Arbitrary_Instance_Predicate_N n = InstanceD context types [mk_Arbitrary_Declaration_N n]
    where
        context = predicateable_instance:arb_instances
        predicateable_instance = ClassP (mkName ("Predicateable"++(show n))) (map VarT (make_names n))
        arb_instances = [ClassP (mkName "Arbitrary") [(VarT (mkName ("x"++(show j))))] | j<- [1..n]]
        predicate_type = (ConT (mkName ("Predicate"++(show n))))
        types = AppT (ConT (mkName "Arbitrary")) (tp n)
        tp 0 = predicate_type
        tp k = AppT (tp (k-1)) (VarT (mkName ("x"++(show k))))
