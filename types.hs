module ZetaTypes where

data Token = TokenAdd
           | TokenSub
           | TokenIntType
           | TokenInt Int
           | TokenEmpty
           deriving(Show, Eq)

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Int Int


instance Show Expr where
    show (Add a b) = show a ++ "Add(" ++ show a ++ ", " ++ show b ++ ")"
    show (Sub a b) = show a ++ " - " ++ show b
    show (Int a) = show a