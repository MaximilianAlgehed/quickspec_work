{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module A where
import qualified Text.Show.Functions
import qualified Data.Typeable as T
import qualified Prelude as P
import qualified QuickSpec as QS
import qualified Test.Feat as F
import qualified Test.QuickCheck as QC
import qualified Tip
data List a = Nil | Cons a (List a)
  deriving (P.Eq, P.Ord, P.Show, T.Typeable)
F.deriveEnumerable (''List)
instance (F.Enumerable a) => QC.Arbitrary (List a) where
  arbitrary = QC.sized F.uniform
data A2 = X | Y deriving (P.Eq, P.Ord, P.Show, T.Typeable)
F.deriveEnumerable (''A2)
instance QC.Arbitrary A2 where arbitrary = QC.sized F.uniform
data R = Nil2 | Eps | Atom A2 | Plus R R | Seq R R | Star R
  deriving (P.Eq, P.Ord, P.Show, T.Typeable)
F.deriveEnumerable (''R)
instance QC.Arbitrary R where arbitrary = QC.sized F.uniform
seq :: R -> R -> R
seq x y =
  case x of
    Nil2 -> Nil2
    _ ->
      case y of
        Nil2 -> Nil2
        _ ->
          case x of
            Eps -> y
            _ ->
              case y of
                Eps -> x
                _ -> Seq x y
plus :: R -> R -> R
plus z y2 =
  case z of
    Nil2 -> y2
    _ ->
      case y2 of
        Nil2 -> z
        _ -> Plus z y2
eqA :: A2 -> A2 -> P.Bool
eqA X X = P.True
eqA X Y = P.False
eqA Y X = P.False
eqA Y Y = P.True
eqList :: List A2 -> List A2 -> P.Bool
eqList Nil Nil = P.True
eqList Nil (Cons z2 x2) = P.False
eqList (Cons x3 xs) Nil = P.False
eqList (Cons x3 xs) (Cons y22 ys) =
  (eqA x3 y22) P.&& (eqList xs ys)
eps :: R -> P.Bool
eps x4 =
  case x4 of
    Eps -> P.True
    Plus p q -> (eps p) P.|| (eps q)
    Seq p2 q2 -> (eps p2) P.&& (eps q2)
    Star y3 -> P.True
    _ -> P.False
epsR :: R -> R
epsR x5 =
  case eps x5 of
    P.True -> Eps
    P.False -> Nil2
step :: R -> A2 -> R
step x6 y4 =
  case x6 of
    Atom b ->
      case eqA b y4 of
        P.True -> Eps
        P.False -> Nil2
    Plus r q3 -> plus (step r y4) (step q3 y4)
    Seq p22 q22 ->
      plus (seq (step p22 y4) q22) (seq (epsR p22) (step q22 y4))
    Star p3 -> seq (step p3 y4) x6
    _ -> Nil2
recognise :: R -> List A2 -> P.Bool
recognise x7 Nil = eps x7
recognise x7 (Cons z3 zs) = recognise (step x7 z3) zs
prop c s =
  (recognise (Atom c) s) Tip.=== (eqList s (Cons c (Nil :: List A2)))
sig =
   QS.signature
     {QS.constants =
        [QS.constant "False" P.False,
         QS.constant "eqA" (eqA :: A2 -> A2 -> P.Bool),
         QS.constant "plus" (plus :: R -> R -> R),
         QS.constant "seq" (seq :: R -> R -> R),
         QS.constant "step" (step :: R -> A2 -> R),
         QS.constant "eqList" (eqList :: List A2 -> List A2 -> P.Bool),
         QS.constant "recognise" (recognise :: R -> List A2 -> P.Bool),
         QS.constant "True" P.True,
         QS.constant "Nil" (Nil :: List QS.A),
         QS.constant "Cons" (Cons :: QS.A -> List QS.A -> List QS.A),
         QS.constant "X" (X :: A2),
         QS.constant "Y" (Y :: A2),
         QS.constant "eps" (eps :: R -> P.Bool),
         QS.constant "Nil2" (Nil2 :: R),
         QS.constant "Eps" (Eps :: R),
         QS.constant "Atom" (Atom :: A2 -> R),
         QS.constant "Plus" (Plus :: R -> R -> R),
         QS.constant "Seq" (Seq :: R -> R -> R),
         QS.constant "epsR" (epsR :: R -> R),
         QS.constant "Star" (Star :: R -> R)],
      QS.instances =
        [QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable P.Int)),
         QS.inst
           ((QS.Sub QS.Dict) :: (P.Ord QS.A) QS.:- (P.Ord (List QS.A))),
         QS.inst
           ((QS.Sub QS.Dict) ::
              (F.Enumerable QS.A) QS.:- (F.Enumerable (List QS.A))),
         QS.inst
           ((QS.Sub QS.Dict) ::
              (F.Enumerable QS.A) QS.:- (QC.Arbitrary (List QS.A))),
         QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord A2)),
         QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable A2)),
         QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary A2)),
         QS.inst ((QS.Sub QS.Dict) :: () QS.:- (P.Ord R)),
         QS.inst ((QS.Sub QS.Dict) :: () QS.:- (F.Enumerable R)),
         QS.inst ((QS.Sub QS.Dict) :: () QS.:- (QC.Arbitrary R))],
      QS.maxTermSize = P.Just (7),
      QS.maxTermDepth = P.Just (4),
      QS.testTimeout = P.Just (1000000)}
