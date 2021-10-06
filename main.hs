import TermParser ( stringToRule, stringToTerm )
import TermEvaluator (matchRule, evalTerm)
import Term (Term, Substitution)

import Data.Maybe (mapMaybe)


splitOn :: String -> [String] -> [[String]]
splitOn _ []= []
splitOn s l = case break (==s) l of
    (pre, []) -> [pre]
    (xs, a:as) -> xs : splitOn s as

evalFile :: [[String]] -> Maybe Term 
evalFile [rs, [t]] = do
    term <- stringToTerm t
    return $ evalTerm (mapMaybe stringToRule rs) term
evalFile _ = Nothing

main :: IO()
main = do
    file <- readFile "trs.txt"
    case evalFile $ splitOn "" $ lines file of 
        Nothing -> print ""
        Just sub -> print sub