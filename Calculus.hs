module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  fromInteger x = Val (fromInteger x)
  negate = UnApp Neg
  (+) = BinApp Add
  (*) = BinApp Mul
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined

instance Fractional Exp where
  fromRational x = Val (fromRational x)
  (/) = BinApp Div
-- Leave the following one undefined...
  recip        = undefined

instance Floating Exp where
  sin = UnApp Sin
  cos = UnApp Cos
  log = UnApp Log
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp a b = fromJust (lookup a b)

showExp :: Exp -> String
showExp (Val x) = show x
showExp (Id x) = x
showExp (UnApp op exp) = lookUp op unopmap ++ "(" ++ showExp exp ++ ")"
  where 
    unopmap = [(Neg, "-"), (Sin, "sin"), (Cos, "cos"), (Log, "log")]
showExp (BinApp op exp1 exp2) = "(" ++ showExp exp1 ++ lookUp op binopmap ++ showExp exp2 ++ ")"
  where
    binopmap = [(Add, "+"), (Mul, "*"), (Div, "/")]

eval :: Exp -> Env -> Double
eval (Val x) env = x
eval (Id x) env = lookUp x env
eval (UnApp op exp) env = lookUp op unopmap (eval exp env)
  where 
    unopmap = [(Neg, negate), (Sin, sin), (Cos, cos), (Log, log)]
eval (BinApp op exp1 exp2) env = lookUp op binopmap (eval exp1 env) (eval exp2 env)
  where 
    binopmap = [(Add, (+)), (Mul, (*)), (Div, (/))]

{-
diff :: Exp -> String -> Exp
diff (Val c) res = Val 0.0
diff (Id x) res 
  | x == res = Val 1.0
  | otherwise = Val 0.0
diff (BinApp op exp1 exp2) res
  | op == Add = BinApp Add (diff exp1 res) (diff exp2 res)
  | op == Mul = BinApp Add (BinApp Mul exp1 (diff exp2 res)) (BinApp Mul (diff exp1 res) exp2) 
  | op == Div = BinApp Div (BinApp Add (BinApp Mul (diff exp1 res) exp2) (UnApp Neg (BinApp Mul exp1 (diff exp2 res)))) (BinApp Mul exp2 exp2)
diff (UnApp op exp) res 
  | op == Sin = BinApp Mul (UnApp Cos exp) (diff exp res)
  | op == Cos = UnApp Neg (BinApp Mul (UnApp Sin exp) (diff exp res))
  | op == Log = BinApp Div (diff exp res) exp
  | op == Neg = UnApp Neg (diff exp res)
-}
diff :: Exp -> String -> Exp
diff (Val c) res = Val 0.0
diff (Id x) res
  | x == res = Val 1.0
  | otherwise = Val 0.0
diff (BinApp op exp1 exp2) res
  | op == Add = diff exp1 res + diff exp2 res
  | op == Mul = exp1 * diff exp2 res + diff exp1 res * exp2
  | op == Div = (diff exp1 res * exp2 - exp1 * diff exp2 res) / (exp2 * exp2)
diff (UnApp op exp) res
  | op == Sin = cos exp * diff exp res
  | op == Cos = negate (sin exp * diff exp res)
  | op == Log = diff exp res / exp
  | op == Neg = negate (diff exp res)


factorial :: Double -> Double
factorial 0 = 1
factorial n = n * factorial (n - 1)

maclaurin :: Exp -> Double -> Int -> Double
maclaurin exp x n = sum (zipWith (\a b -> b * (x ** a) / factorial a) listcount listF)
  where
    listF = map (`eval` [("x", 0)]) (take n (iterate (`diff` "x") exp))
    listcount = take n (iterate (+1) 0)
---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
