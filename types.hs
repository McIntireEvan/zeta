module Zeta.Types where

data Token = TokAdd
           | TokSub
           | TokMult
           | TokDiv
           | TokIntType
           | TokInt (Maybe Int)
           | TokEmpty
           deriving(Show, Eq)

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Int (Maybe Int)

data ExprType = Unary
              | Binary
              deriving (Show, Eq)

instance Show Expr where
    show (Add a b) = "Add(" ++ show a ++ ", " ++ show b ++ ")"
    show (Sub a b) = "Sub(" ++ show a ++ ", " ++ show b ++ ")"
    show (Mult a b) = "Mult(" ++ show a ++ ", " ++ show b ++ ")"
    show (Div a b) = "Div(" ++ show a ++ ", " ++ show b ++ ")"
    show (Int a) = let Just s = a in show s