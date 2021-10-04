import TermParser ( stringToRule, stringToTerm )
import TermEvaluator (matchRule)
import Term (Term, Substitution)

import Data.Maybe (mapMaybe)


splitOn :: String -> [String] -> [[String]]
splitOn _ []= []
splitOn s l = case break (==s) l of
    (pre, []) -> [pre]
    (xs, a:as) -> xs : splitOn s as

matchTerm :: [[String]] -> Maybe Substitution
matchTerm [rs, [t]] = do
    term <- stringToTerm t
    matchRule (mapMaybe stringToRule rs) term
matchTerm _ = Nothing

main :: IO()
main = do
    file <- readFile "trs.txt"
    case matchTerm $ splitOn "" $ lines file of 
        Nothing -> print ""
        Just sub -> print sub