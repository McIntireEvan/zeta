import Text.Regex.PCRE
import ZetaTypes

regex_pairs = [
        ("(\\+)", TokenAdd),
        ("(\\-)", TokenSub),
        ("(-?[0-9]+)", TokenIntType),
        ("([ \t\n\r])", TokenEmpty)
    ]

lexString :: String -> [Token]
lexString [] = []
lexString str = let (rem, tok) = getTok str in
    if tok == TokenEmpty
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
selectToken token matched
    | token == TokenIntType = (TokenInt (read matched :: Int))
    | otherwise = token