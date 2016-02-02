{-# LANGUAGE TemplateHaskell #-}

module TemplateDerivingPredicates where

import Language.Haskell.TH
import Test.QuickCheck
import Data.Typeable
import Data.List

-- Get the "basic types" (i.e. not (,), (->), etc)
get_types :: Type -> [Type]
get_types (ForallT _ _ t) = get_types t
get_types (AppT t1 t2) = (get_types t1) ++ (get_types t2)
get_types t@(ConT _) = [t]
get_types t@(PromotedT _) = [t]
get_types _ = []

-- Get the number of interesting types
type_count :: Type -> Integer
type_count (ForallT _ _ t) = type_count t
type_count (AppT t1 t2) = (type_count t1) + (type_count t2)
type_count (SigT t _) = type_count t
type_count (VarT _) = 1
type_count (ConT _) = 1
type_count (PromotedT _) = 1
type_count _ = 0

-- Check if an expression is a type signature
is_sig :: Exp -> Bool
is_sig (SigE _ _) = True
is_sig _ = False

mk_Predicates = mk_Predicates' True
mk_Predicates_No_Ord = mk_Predicates' False

-- Creates a t-type from a list of quoted transformations and predicates
mk_Transforms :: [(ExpQ, ExpQ)] -> Q [Dec]
mk_Transforms exprs' = do
                        let exprsL = sequence (fst (unzip exprs')) :: Q [Exp]
                        let exprsR = sequence (snd (unzip exprs')) :: Q [Exp]
                        lstL <- exprsL
                        lstR <- exprsR
                        let exprs = filter (\(x, y) -> (is_sig x) && (is_sig y)) (zip lstL lstR)
                        t_types <- mk_Ttypes exprs
                        arbitraries <- mk_Ttypes_Arbitrary exprs
                        return (t_types++arbitraries)

mk_Ttypes_Arbitrary :: [(Exp, Exp)] -> Q [Dec]
mk_Ttypes_Arbitrary exprs = sequence $ map (\(p, t) -> return $ InstanceD [] (AppT (ConT (mkName "Arbitrary")) (ConT (name_ p t))) [dec p t]) exprs
    where
        dec pred transform = FunD (mkName "arbitrary") [Clause [] (NormalB (doexpr pred transform))[]]
    
        doexpr pred transform = DoE [BindS (VarP (mkName "x")) (AppE (AppE (VarE (mkName "suchThat")) (VarE (mkName "arbitrary"))) (sigs_to_combination pred transform)),
                                     NoBindS (AppE (VarE (mkName "return")) (AppE (AppE (ConE (name_ pred transform)) (VarE (mkName "x"))) (AppE (VarE (getName transform)) (VarE (mkName "x")))))]

        getName (SigE (VarE name) _) = mkName $ nameBase name
        name_ pred transform = mkName ("T"++(nameBase (getName pred))++(nameBase (getName transform)))

mk_Ttypes :: [(Exp, Exp)] -> Q [Dec]
mk_Ttypes exprs = sequence $ map (\(p, t) -> return (DataD [] (name_ p t) [] [record p t] instances)) exprs
                                where
                                    instances = [''Ord, ''Show, ''Eq]
                                    name_ pred transform = mkName ("T"++(nameBase (getName pred))++(nameBase (getName transform)))
                                    getName (SigE (VarE name) _) = name
                                    
                                    record pred transform = RecC (name_ pred transform) [(mkName ("x"++(nameBase (name_ pred transform))), NotStrict, getType transform), (mkName ("xt"++(nameBase (name_ pred transform))), NotStrict, getType pred)]
                                    
                                    getType (SigE _ (AppT (AppT _ t) _)) = t

sigs_to_combination :: Exp -> Exp -> Exp
sigs_to_combination (SigE p _) (SigE t _) = LamE [VarP (mkName "x")] (AppE p (AppE t (VarE (mkName "x"))))

-- Creates all the plumbing given a list of quoted signatures
mk_Predicates' :: Bool -> [ExpQ] -> Q [Dec]
mk_Predicates' ord exprs = do
                        exprs' <- fmap (filter is_sig) $ sequence exprs
                        predicate_types <- fmap concat $ sequence $ map (mk_Predicate_Types . (\x -> x-1)) $ get_type_counts exprs'
                        newtypes <- fmap concat $ sequence $ map (mk_Newtypes ord) $ nub $ get_types_per_expr exprs'
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

                            get_type_counts :: [Exp] -> [Integer]
                            get_type_counts xs = filter (>0) $ nub $ map (type_count . get_type) $ filter is_sig xs

                            get_type :: Exp -> Type
                            get_type (SigE _ t) = t

-- Takes a name and a type and creates the name of a newtype that wraps the type
-- and adds the name to make it a unique new name
make_compound_name :: Name -> Type -> Name
make_compound_name n t = (mkName ("T"++(nameBase n) ++ (case t of
                                                        (ConT name) -> nameBase name
                                                        (PromotedT name) -> nameBase name)))

-- Creates the newtypes associated with one predicate
mk_Newtypes :: Bool -> (Name, [Type]) -> Q [Dec]
mk_Newtypes ord (n, tps) = return $ (nub (concat (map helper tps)))++(predicateable_instance n tps)
    where
        predicateable_instance :: Name -> [Type] -> [Dec]
        predicateable_instance n tps = [InstanceD [] inst_type [dec]]
            where
                inst_type = foldl AppT (ConT (mkName ("Predicateable"++(show(length tps))))) (map (ConT . (make_compound_name n)) tps)
                conds = [ConP (make_compound_name n t) [(VarP (mkName ("x"++(show i))))] | 
                                                                            (t, i) <- (zip tps [0..])]
                n' = (mkName ("predicate"++(show (length tps))))
                body = foldl AppE (VarE n) [VarE (mkName ("x"++(show i))) | i <- [0..((length tps)-1)]]
                dec = FunD n' [Clause conds (NormalB body) []]
        helper :: Type -> [Dec]
        helper t = let n' = make_compound_name n t in 
                       [NewtypeD [] n' [] (NormalC n' [(NotStrict, t)]) ((if ord then [''Ord] else [])++[''Eq, ''Typeable, ''Arbitrary])]

-- Creates a type synonym for predicae2 instance
mk_TypeSynonym :: (Name, Integer, [Type]) -> Q [Dec]
mk_TypeSynonym (n, i, tps) = return [TySynD (mkName ("P"++(nameBase n))) [] t]
    where
        t = foldl AppT (ConT (mkName ("Predicate"++(show i)))) (map diffType (reverse (tail (reverse tps))))
        diffType t = ConT (make_compound_name n t)

-- Creates the entire predicate derivation structure when spliced
mk_Predicate_Types :: Integer -> Q [Dec]
mk_Predicate_Types n = do
     m <- lookupTypeName ("Predicate"++(show n))
     case m of
        Nothing -> return [mk_Predicate_N n,
                           mk_Predicateable_N n,
                           mk_Arbitrary_Instance_Predicate_N n
                   ]
        _ -> return []

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
mk_Arbitrary_Function_Call_N n =
                                (AppE
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
        predicateable_instance = foldl AppT (ConT (mkName ("Predicateable"++(show n)))) (map VarT (make_names n))
        arb_instances = [foldl AppT (ConT (mkName "Arbitrary")) [(VarT (mkName ("x"++(show j))))] | j<- [1..n]]
        predicate_type = (ConT (mkName ("Predicate"++(show n))))
        types = AppT (ConT (mkName "Arbitrary")) (tp n)
        tp 0 = predicate_type
        tp k = AppT (tp (k-1)) (VarT (mkName ("x"++(show k))))
