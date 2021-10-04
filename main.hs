import TermParser ( stringToRule, stringToTerm )
import TermEvaluator (matchRule, singleEvalStep)
import Term (Term, Substitution)

import Data.Maybe (mapMaybe)


splitOn :: String -> [String] -> [[String]]
splitOn _ []= []
splitOn s l = case break (==s) l of
    (pre, []) -> [pre]
    (xs, a:as) -> xs : splitOn s as

evalTerm :: [[String]] -> Maybe Term 
evalTerm [rs, [t]] = do
    term <- stringToTerm t
    singleEvalStep (mapMaybe stringToRule rs) term
evalTerm _ = Nothing

main :: IO()
main = do
    file <- readFile "trs.txt"
    case evalTerm $ splitOn "" $ lines file of 
        Nothing -> print ""
        Just sub -> print sub