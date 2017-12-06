module Zeta.Parser where
import Zeta.Types

{-|
  Precedence of tokens. Those on the same level have the same precedence and
  number of things they operate on (unary = 1, binary = 1, etc.). The further
  down a token, the higher precedence it has.
|-}
precedence = [
    ([TokAdd, TokSub], Binary),
    ([TokMult, TokDiv], Binary),
    ([TokInt Nothing], Unary)
    ]

-- | Matches a given taken and removes it from the list
matchToken :: [Token] -> Token -> [Token]
matchToken toks match =
    let (h:t) = toks in
        if h == match then t
        else error "Malformed expression"

-- | Parses a token list, returning an expression and remaining tokens
parse :: [Token] -> (Expr, [Token])
parse toks = parseToks toks precedence

{-|
  The bulk of the parser. Has a special case for the integer level.
  For all other levels, it goes as deep into precedence as it can and tries to
  match something, and works it way back up, using that expression and others as
  needed. This style of recursion works with the precedence list to establish
  an order of operations.
  TODO: Move special case into general code
  TODO: Type checking before calling parseInt
  TODO: Cleanup variable names/formatting
|-}
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

-- | Turns an Int token into an expression
-- | TODO: Expand into a `matchUnary` function
parseInt :: [Token] -> Expr
parseInt toks =
    let TokInt(Just s) = (head toks) in Int(Just s)

-- | Creates a binary expression given a token
matchBinary :: Token -> Expr -> Expr -> Expr
matchBinary tok e1 e2
    | tok == TokAdd = Add e1 e2
    | tok == TokSub = Sub e1 e2
    | tok == TokMult = Mult e1 e2
    | tok == TokDiv = Div e1 e2