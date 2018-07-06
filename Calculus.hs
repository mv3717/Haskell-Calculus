module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

type Operators = Either UnOp BinOp

-- Pre: we assume that all look-ups succed.
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key search
  = fromJust (lookup key search)

-- This function is given an expression and an "environment" and evaluates
-- the expression.
eval :: Exp -> Env -> Double
eval (Val exp) env              = exp
eval (Id exp) env               = lookUp exp env
eval (UnApp Neg exp) env        = -(eval exp env)
eval (UnApp Sin exp) env        = sin (eval exp env)
eval (UnApp Cos exp) env        = cos (eval exp env)
eval (UnApp Log exp) env        = log (eval exp env)
eval (BinApp Add exp1 exp2) env = (eval exp1 env) + (eval exp2 env)
eval (BinApp Mul exp1 exp2) env = (eval exp1 env) * (eval exp2 env)
eval (BinApp Div exp1 exp2) env = (eval exp1 env) / (eval exp2 env)

-- applies the differentiation rules to a given expression.
diff :: Exp -> String -> Exp
diff (Val x) variable
  = Val 0
diff (Id string) variable
 | string == variable = Val 1
 | otherwise = Val 0
diff (UnApp Neg exp) variable
  = UnApp Neg (diff exp variable)
diff (UnApp Sin exp) variable
  = BinApp Mul term1 term2
    where
      term1 = UnApp Cos exp
      term2 = diff exp variable
diff (UnApp Cos exp) variable
  = UnApp Neg (BinApp Mul term1 term2)
    where
      term1 = UnApp Sin exp
      term2 = diff exp variable
diff (UnApp Log exp) variable
  = BinApp Div (diff exp variable) exp
diff (BinApp Add exp1 exp2) variable
  = BinApp Add term1 term2
    where
      term1 = diff exp1 variable
      term2 = diff exp2 variable
diff (BinApp Mul exp1 exp2) variable
  = BinApp Add term1 term2
    where
      term1 = BinApp Mul exp1 (diff exp2 variable)
      term2 = BinApp Mul (diff exp1 variable) exp2
diff (BinApp Div exp1 exp2) variable
  = BinApp Div term1 term2
    where
      term2 = BinApp Mul exp2 exp2
      term1 = BinApp Add term1' term1''
        where
          term1'  = BinApp Mul (diff exp1 variable) exp2
          term1'' = UnApp Neg  (BinApp Mul (diff exp2 variable) exp1)

-- approximates the value of a function at point x using the first n terms
-- of the Maclaurin series.
maclaurin :: Exp -> Double -> Int -> Double
maclaurin exp x 1
  = (eval exp [("x", 0)])
maclaurin exp x n
  = diffValue*(x^(n-1))/(fromIntegral (factorial (n-1))) + maclaurin exp x (n-1)
    where
      diffValue  = eval (last listOfDiff) [("x", 0)]
      listOfDiff = take n (iterate diff' exp)

-- We use this function to call diff. The reason for this is that the function
-- iterate only accepts a one input function and a variable. Therefore,
-- I use this function to set the variable name as "x" and then differentiate
-- the expression with the regular diff function.
diff' :: Exp -> Exp
diff' exp
  = diff exp "x"

-- returns n!
factorial :: Int -> Int
factorial 0
  = 1
factorial n
  = n*factorial(n-1)

-- generates a neat printable representation for expressions.
showExp :: Exp -> String
showExp (Val x)
  = show x ++ ""
showExp (Id x)
  = x ++ ""
showExp (UnApp Neg exp)
  = "-(" ++ showExp exp ++ ")"
showExp (UnApp Sin exp)
  = "sin(" ++ showExp exp ++ ")"
showExp (UnApp Cos exp)
  = "cos(" ++ showExp exp ++ ")"
showExp (UnApp Log exp)
  = "log(" ++ showExp exp ++ ")"
showExp (BinApp Add exp1 exp2)
  = "(" ++ (showExp exp1) ++ "+" ++ (showExp exp2) ++ ")"
showExp (BinApp Mul exp1 exp2)
  = "(" ++ (showExp exp1) ++ "*" ++ (showExp exp2) ++ ")"
showExp (BinApp Div exp1 exp2)
  = "(" ++ (showExp exp1) ++ "/" ++ (showExp exp2) ++ ")"

---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

----------------------------------------------------------------------
-- EXTENSION: Uncomment and complete these...

instance Num Exp where
  negate x = UnApp Neg x
  x + y = BinApp Add x y
  x * y = BinApp Mul x y

instance Fractional Exp where
  x / y = BinApp Div x y

instance Floating Exp where
  sin x = UnApp Sin x
  cos x = UnApp Cos x
  log x = UnApp Log x

-- instance (Eq a, Num a) => Num (Maybe a) where

-- instance (Eq a, Fractional a) => Fractional (Maybe a) where

-- diff2 :: Exp -> String -> Maybe Exp



-- The following makes it much easier to input expressions, e.g. sin x, log(x*x) etc.

x, y :: Exp
x = Id "x"
y = Id "y"
