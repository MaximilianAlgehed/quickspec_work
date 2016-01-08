{-# LANGUAGE TemplateHaskell #-}

module TemplateDerivingFunctions where

import Language.Haskell.TH
import Test.QuickCheck
import Data.Typeable
import Data.List
import System.IO.Unsafe

-- Get the number of interesting types
type_count :: Type -> Integer
type_count (ForallT _ _ t) = type_count t
type_count (AppT t1 t2) = (type_count t1) + (type_count t2)
type_count (SigT t _) = type_count t
type_count (VarT _) = 1
type_count (ConT _) = 1
type_count (PromotedT _) = 1
type_count _ = 0

type_count' t = (type_count t) - 1 

-- Check if an expression is a type signature
is_sig :: Exp -> Bool
is_sig (SigE _ _) = True
is_sig _ = False

-- Creates a function whenPredicate
mk_When_Function :: ExpQ -> ExpQ -> Q [Dec]
mk_When_Function expr working =
     do
        s <- expr
        case s of
            (SigE (VarE name) types) -> do
                t <- working 
                --t <- unsafe_hack
                return ([FunD (mkName ("when_"++(nameBase name))) [(Clause [tup_patterns (type_count' types)] (body t) [])]] ++ accs ++ elems)
                where
                    unwrap = return $ LamE [tup_patterns (type_count' types)] (applications (type_count' types))

                    unsafe_hack = [| head $ unsafePerformIO $ sample' (arbitrary `suchThat` $(unwrap)) |]
                    
                    tup_patterns 1 = VarP (mkName "x1")
                    tup_patterns n = TupP [VarP (mkName ("x"++(show k))) | k <- [1..n]]
                    
                    tup_exprs 1 = VarE (mkName "x1")
                    tup_exprs n = TupE [VarE (mkName ("x"++(show k))) | k <- [1..n]]
        
                    body t = GuardedB [whenNot, ow t]
                    applications 0 = VarE (name)
                    applications k = AppE (applications (k-1)) (VarE (mkName ("x"++(show k))))
                    whenNot = (NormalG (applications (type_count' types)), tup_exprs (type_count' types))
                    ow t = (NormalG (ConE (mkName "True")), t)    
                    accs = [FunD (mkName ("acc"++(show n))) [Clause [tup_patterns (type_count' types)] (NormalB (VarE (mkName ("x"++(show n))))) []] | n <- [1..(type_count' types)]]
                    elems = [FunD (mkName ("when_"++(nameBase name)++(show n))) [Clause [] (NormalB (AppE (AppE (VarE (mkName ".")) (VarE (mkName ("acc"++(show n))))) (VarE (mkName ("when_"++(nameBase name)))))) []] | n <- [1..(type_count' types)]]
            _ -> return []
