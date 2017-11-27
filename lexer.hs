import Text.Regex.PCRE
import ZetaTypes

regex_pairs = [
        ("(\\+)", TokAdd),
        ("(\\-)", TokSub),
        ("(\\*)", TokMult),
        ("(\\/)", TokDiv),
        ("(-?[0-9]+)", TokIntType),
        ("([ \t\n\r])", TokEmpty)
    ]

lexString :: String -> [Token]
lexString [] = []
lexString str = let (rem, tok) = getTok str in
    if tok == TokEmpty
        then lexString rem
        else tok:(lexString rem)

getTok :: String -> (String, Token)
getTok str = select (prune (map (matchStr str) regex_pairs))

matchStr :: String -> (String, Token) -> (String, Bool, String, Token)
matchStr str (regex, tok) =
    let matched = (str =~ ("^" ++ regex) :: Bool) in
    let sub = (if matched then (str =~ ("^" ++ regex) :: String) else "")
    in (str, matched, sub, tok)

prune :: [(String, Bool, String, Token)] -> (String, Bool, String, Token)
prune [] = error "Invalid Input"
prune ((str, matched, sub, tok):tail) =
    if matched then (str, matched, sub, tok) else prune tail

select :: (String, Bool, String, Token) -> (String, Token)
select (s, b, h, v) =
    let l = length h in (drop l s, (selectToken v h))

selectToken :: Token -> String -> Token
selectToken tok matched
    | tok == TokIntType = (TokInt (read matched :: Int))
    | otherwise = tok