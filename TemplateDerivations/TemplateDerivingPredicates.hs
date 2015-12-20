{-# LANGUAGE TemplateHaskell #-}

module TemplateDerivingPredicates where

import Language.Haskell.TH
import Test.QuickCheck
import Data.Typeable
import Data.List

-- Creates all the plumbing given a list of quoted signatures
mk_Predicates :: [ExpQ] -> Q [Dec]
mk_Predicates exprs = do
                        exprs' <- sequence exprs
                        predicate_types <- fmap concat $ sequence $ map (mk_Predicate_Types . (\x -> x-1)) $ get_type_counts exprs'
                        newtypes <- fmap concat $ sequence $ map mk_Newtypes $ nub $ get_types_per_expr exprs'
                        synonyms <- fmap concat $ sequence $ map mk_TypeSynonym $ get_names_per_expr exprs'
                        return (predicate_types++synonyms++newtypes)
                        where
                            get_names_per_expr :: [Exp] -> [(Name, Integer, [Type])]
                            get_names_per_expr [] = []
                            get_names_per_expr ((SigE (VarE n) t):xs) = (n, (type_count t) - 1, get_types t):(get_names_per_expr xs)
                        
                            get_types_per_expr :: [Exp] -> [(Name, [Type])]
                            get_types_per_expr = (map  get_name_and_types) . (filter is_sig)

                            get_name_and_types :: Exp -> (Name, [Type])
                            get_name_and_types (SigE (VarE n) t) = (n, ((reverse . tail . reverse)  (get_types t)))

                            get_types :: Type -> [Type]
                            get_types (ForallT _ _ t) = get_types t
                            get_types (AppT t1 t2) = (get_types t1) ++ (get_types t2)
                            get_types t@(ConT _) = [t]
                            get_types t@(PromotedT _) = [t]
                            get_types _ = []

                            get_type_counts :: [Exp] -> [Integer]
                            get_type_counts xs = filter (>0) $ nub $ map (type_count . get_type) $ filter is_sig xs

                            is_sig :: Exp -> Bool
                            is_sig (SigE _ _) = True
                            is_sig _ = False

                            get_type :: Exp -> Type
                            get_type (SigE _ t) = t

                            type_count :: Type -> Integer
                            type_count (ForallT _ _ t) = type_count t
                            type_count (AppT t1 t2) = (type_count t1) + (type_count t2)
                            type_count (SigT t _) = type_count t
                            type_count (VarT _) = 1
                            type_count (ConT _) = 1
                            type_count (PromotedT _) = 1
                            type_count _ = 0

mk_Newtypes :: (Name, [Type]) -> Q [Dec]
mk_Newtypes (n, tps) = return $ (nub (concat (map helper tps)))++(predicateable_instance n tps)
    where
        make_compound_name :: Type -> Name
        make_compound_name t = (mkName ("T"++(nameBase n) ++ (case t of
                                                        (ConT name) -> nameBase name
                                                        (PromotedT name) -> nameBase name)))
        predicateable_instance :: Name -> [Type] -> [Dec]
        predicateable_instance n tps = [InstanceD [] inst_type [dec]]
            where
                inst_type = foldl AppT (ConT (mkName ("Predicateable"++(show(length tps))))) (map (ConT . make_compound_name) tps)
                conds = [ConP (make_compound_name t) [(VarP (mkName ("x"++(show i))))] | 
                                                                            (t, i) <- (zip tps [0..])]
                n' = (mkName ("predicate"++(show (length tps))))
                body = foldl AppE (VarE n) [VarE (mkName ("x"++(show i))) | i <- [0..((length tps)-1)]]
                dec = FunD n' [Clause conds (NormalB body) []]
        helper :: Type -> [Dec]
        helper t = let n' = make_compound_name t in 
                       [NewtypeD [] n' [] (NormalC n' [(NotStrict, t)]) [''Ord, ''Eq, ''Typeable], arb_instance t n']
        arb_instance :: Type -> Name -> Dec
        arb_instance t n = InstanceD [] (AppT (ConT (mkName "Arbitrary")) (ConT n)) [arb]
            where
                arb = FunD (mkName "arbitrary") [Clause [] (NormalB (AppE (AppE (VarE (mkName "fmap")) (ConE n)) (VarE (mkName "arbitrary")))) []]

mk_TypeSynonym :: (Name, Integer, [Type]) -> Q [Dec]
mk_TypeSynonym (n, i, tps) = return [TySynD (mkName ("P"++(nameBase n))) [] t]
    where
        t = foldl AppT (ConT (mkName ("Predicate"++(show i)))) (map diffType (reverse (tail (reverse tps))))
        diffType t = ConT (mkName ("T"++(nameBase n) ++ (case t of
                                                        (ConT name) -> nameBase name
                                                        (PromotedT name) -> nameBase name)))

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
