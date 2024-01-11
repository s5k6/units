module AST where



data Operator
  = Add | Sub | Mul | Div | Pow
  deriving Show

data Unit = Unit [(String, Word)] [(String, Word)]

instance Show Unit where
  showsPrec _  (Unit [] []) = id
  showsPrec _  (Unit _as _bs) = showChar '[' . showChar ']'



data Expression
  = Quantity Int Unit
  | Apply Operator Expression Expression
  deriving Show



eval :: Expression -> Int

eval e = case e of
  Quantity v _ -> v
  Apply o x y -> apply o (eval x) (eval y)

  where
    apply o x y = case o of
      Add -> x + y
      Sub -> x - y
      Mul -> x * y
      Div -> x `div` y
      Pow -> x ^ y
