import TermParser ( stringToTerm, parseTRS )
import TermEvaluator  (evalTerm)
import Term (Term, Substitution)
import CommandParser (stringToCommand)

import Data.Maybe (mapMaybe)


splitOn :: String -> [String] -> [[String]]
splitOn _ []= []
splitOn s l = case break (==s) l of
    (pre, []) -> [pre]
    (xs, a:as) -> xs : splitOn s as

evalFile :: [[String]] -> Maybe Term 
evalFile [rs, [t]] = do
    term <- stringToTerm t
    trs <- parseTRS rs
    return $ evalTerm trs term
evalFile _ = Nothing

main :: IO()
main = do
    inp <- getLine 
    case stringToCommand inp of 
        Nothing -> print "Error"
        Just cmd -> print cmd
    main

    {--file <- readFile "trs.txt"
    case evalFile $ splitOn "" $ lines file of 
        Nothing -> print ""
        Just sub -> print sub
    --}