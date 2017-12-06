module Zeta.Parser where
import Zeta.Types

-- The more towards the bottom, the higher the precedence. Numbers have highest
-- Items on the same level have the same precedence
precedence = [
    ([TokAdd, TokSub], Binary),
    ([TokMult, TokDiv], Binary),
    ([TokInt Nothing], Unary)
    ]

matchToken :: [Token] -> Token -> [Token]
matchToken toks match =
    let (h:t) = toks in
        if h == match then t
        else error "Malformed expression"

parse :: [Token] -> (Expr, [Token])
parse toks = parseToks toks precedence

parseToks :: [Token] -> [([Token], ExprType)] -> (Expr, [Token])
parseToks toks [] = error "Cannot match"
parseToks toks [([TokInt Nothing], Unary)] =
    (parseInt toks, matchToken toks (head toks))
parseToks toks prec =
    let ((l,etype):t) = prec in
    let (exp, rem) = parseToks toks t
    in if elem (head rem) l then
        if etype == Binary then
            let c = (head rem) in
            let r1 = matchToken rem c in
            let (exp2, r2) = parseToks r1 ((l,etype):t) in
                (matchBinary c exp exp2, r2)
        else (exp, rem)
    else (exp, rem)

parseInt :: [Token] -> Expr
parseInt toks =
    let TokInt(Just s) = (head toks) in Int(Just s)

matchBinary :: Token -> Expr -> Expr -> Expr
matchBinary tok e1 e2
    | tok == TokAdd = Add e1 e2
    | tok == TokSub = Sub e1 e2
    | tok == TokMult = Mult e1 e2
    | tok == TokDiv = Div e1 e2