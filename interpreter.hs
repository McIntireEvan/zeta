module Zeta.Interpreter where
import Zeta.Types
import Zeta.Parser
import Zeta.Lexer

-- | Evaluates an expression into an Int
eval :: Expr -> Int
eval expr = case expr of
    (Add e1 e2) -> eval(e1) + eval(e2)
    (Sub e1 e2) -> eval(e1) - eval(e2)
    (Mult e1 e2) -> eval(e1) * eval(e2)
    (Div e1 e2) -> let ex2 = eval(e2) in
        if ex2 == 0 then error "Div by zero" else eval(e1) `div` ex2
    Int(Just s) -> s

-- | Strings the lexer, parser, and interpreter together
-- | TODO: Move?
run :: String -> Int
run str = let (expr, toks) = parse (lexString str) in eval(expr)