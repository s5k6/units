module Main ( main ) where

{- Ideas on a simple unit system -}

names = ["m", "kg", "s", "A", "K"]

ampere x = Q x [0,0,0,1,0]
volt x = Q x [] * watt 1 / ampere 1
watt x = Q x [2,1,-3]

data Quantiified a = Q a [Int]

instance Show a => Show (Quantiified a) where
  show (Q x us) = show x ++ " [" ++ unit ++ "]"
    where
      named = zip us names
      pos = [ fmt u n | (u,n) <- named, u > 0]
      neg = [ fmt (negate u) n | (u,n) <- named, u < 0]
      fmt u n | u > 1 = n ++ show u
              | otherwise = n
      unit | length neg == 0 = unwords pos
           | length pos > 0 = unwords $ pos ++ "/" : neg
           | otherwise = unwords $ "1 /" : neg

(.*.) :: [Int] -> [Int] -> [Int]
(.*.) (x:xs) (y:ys) = x + y : xs .*. ys
(.*.) xs [] = xs
(.*.) [] ys = ys

(./.) :: [Int] -> [Int] -> [Int]
(./.) xs ys = xs .*. map negate ys

instance Num a => Num (Quantiified a) where
  Q x us + Q y vs | us == vs = Q (x+y) us
                  | otherwise = error "Mismatch"
  Q x us * Q y vs = Q (x * y) (us .*. vs)
  abs (Q x us) = Q (abs x) us
  signum (Q x _) = Q (signum x) []
  fromInteger x = Q (fromInteger x) []
  negate (Q x us) = Q (negate x) us

instance Fractional a => Fractional (Quantiified a) where
  Q x us / Q y vs = Q (x / y) (us ./. vs)
  fromRational x = Q (fromRational x) []

main = do
  f (Q 2 [1]) plus (Q 3 [1])
  f (Q 5 [0,1]) minus (Q 3 [0,1])
  f (Q 2 [1]) times (Q 3 [1])
  f (Q 2 [1,0]) times (Q 3 [0,1])
  f (Q 15 [3]) by (Q 5 [1])
  f (Q 6 [2,3]) by (Q 5 [0,2])
  f (fromInteger 2) times (Q 3 [0,0,-1])
  f (Q 2 [1,0,-1]) by (Q 4 [0,0,1])
  f (volt 12) times (ampere 3)
  f (watt 3) by (volt 5)
  where
    f a (o, (#)) b = putStrLn $ unwords [show a, o, show b, "=", show $ a # b]

    plus = ("+", (+))
    minus = ("-", (-))
    times = ("*", (*))
    by = ("/", (/))
