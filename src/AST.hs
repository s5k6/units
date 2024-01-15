module AST where

import qualified Data.Map as M
import Data.List ( intersperse )



compose :: Foldable f => f (c -> c) -> c -> c

compose = foldl (.) id



data Operator
  = Add | Sub | Mul | Div | Pow
  deriving Show



data Unit = Unit (M.Map String Int)
  deriving Eq

instance Show Unit where
  showsPrec _ (Unit um) = case (above, below) of
    ([], []) -> id
    ([], bs) -> brk $ showString "/ " . line bs
    (as, []) -> brk $ line as
    (as, bs) -> brk $ line as . showString " / " . line bs

    where
      us = M.toList um
      above = [ u | u <- us, snd u > 0 ]
      below = [ (n, negate c) | (n,c) <- us, c < 0 ]
      line qs = compose $ intersperse (showChar ' ') [ fmt n c | (n, c) <- qs ]
      brk ss = showString "[" . ss . showString "]"
      fmt n c | c > 1 = showString n . shows c
              | otherwise = showString n

instance Num a => Num (Quantified a) where
  Q x u + Q y v
    | u == v = Q (x+y) u
    | otherwise = error $ "Sum of " ++ show u ++ " and " ++ show v
  Q x u * Q y v = Q (x * y) (u .*. v)
  abs (Q x u) = Q (abs x) u
  signum (Q x _) = Q (signum x) one
  fromInteger x = Q (fromInteger x) one
  negate (Q x u) = Q (negate x) u

instance Fractional a => Fractional (Quantified a) where
  Q x u / Q y v = Q (x / y) (u ./. v)
  recip (Q x u) = Q (recip x) (invert u)
  fromRational x = Q (fromRational x) one

data Quantified a = Q a Unit

instance Show a => Show (Quantified a) where
  showsPrec _ (Q v u) = shows v . showChar ' ' . shows u


one :: Unit
one = Unit $ M.empty

isOne :: Unit -> Bool
isOne (Unit u) = M.null u

(.*.) :: Unit -> Unit -> Unit
Unit u .*. Unit v = Unit . M.filter (/=0) $ M.unionWith (+) u v

invert :: Unit -> Unit
invert (Unit u) = Unit $ M.map negate u

(./.) :: Unit -> Unit -> Unit
u ./. v = u .*. invert v

(.^) :: Unit -> Int -> Unit
Unit u .^ n = Unit . M.filter (/=0) $ M.map (* n) u



data Expression
  = Quantity (Quantified Double)
  | Apply Operator Expression Expression
  deriving Show



eval :: Expression -> Quantified Double

eval e = case e of
  Quantity q -> q
  Apply o x y -> apply o (eval x) (eval y)

  where
    apply o x y = case o of
      Add -> x + y
      Sub -> x - y
      Mul -> x * y
      Div -> x / y
      Pow -> pow x y

        where
         pow (Q x u) (Q y v) | f == 0 && isOne v = Q (x ^ n) (u .^ n)
                             | otherwise = error "Invalid exponent"
           where
             (n, f) = properFraction y
