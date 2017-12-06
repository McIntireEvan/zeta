module Zeta.Lexer where
import Text.Regex.PCRE
import Zeta.Types

-- Regular expressions and their associated tokens
-- TODO: Negative numbers don't work
regex_pairs = [
        ("(\\+)", TokAdd),
        ("(\\-)", TokSub),
        ("(\\*)", TokMult),
        ("(\\/)", TokDiv),
        ("(-?[0-9]+)", TokInt Nothing),
        ("([ \t\n\r])", TokEmpty)
    ]

-- | Lexes a string into a list of tokens
lexString :: String -> [Token]
lexString [] = [TokEmpty]
lexString str = let (rem, tok) = getTok str in
    if tok == TokEmpty
        then lexString rem
        else tok:(lexString rem)

-- | Matches the start of a string, returning a token and the rest of the string
getTok :: String -> (String, Token)
getTok str = select (prune (map (matchStr str) regex_pairs))

-- | Checks a string against a regex-token pair
matchStr :: String -> (String, Token) -> (String, Bool, String, Token)
matchStr str (regex, tok) =
    let matched = (str =~ ("^" ++ regex) :: Bool) in
    let sub = (if matched then (str =~ ("^" ++ regex) :: String) else "")
    in (str, matched, sub, tok)

-- | Selects a tuple that matched
prune :: [(String, Bool, String, Token)] -> (String, Bool, String, Token)
prune [] = error "Invalid Input"
prune ((str, matched, sub, tok):tail) =
    if matched then (str, matched, sub, tok) else prune tail

-- | Cleans up the data and does a final selection
select :: (String, Bool, String, Token) -> (String, Token)
select (s, b, h, v) =
    let l = length h in (drop l s, (selectToken v h))

-- | Selects a token, sometimes adding extra info
selectToken :: Token -> String -> Token
selectToken tok matched
    | tok == (TokInt Nothing) = (TokInt (Just (read matched :: Int)))
    | otherwise = tok