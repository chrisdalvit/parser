module CommandEvaluator (evalCommand) where

import Term (Term, Substitution, Rule)
import TermParser ( stringToTerm, stringsToTRS )
import TermEvaluator  (evalTerm)
import CommandParser(Command(..), CommandSymbol (CommandSymbol), Args (Args), stringToCommand)

evalCommand :: Command -> IO ()
evalCommand (Command (CommandSymbol "term") (Args [x])) = do
    print $ map stringToTerm [x]
evalCommand (Command (CommandSymbol "trs") _ ) = do
    trs <- readTRS
    case trs of 
        [] -> print "Non-valid TRS"
        _ -> print trs
evalCommand (Command (CommandSymbol "file") (Args [fn])) = do
    file <- readFile fn
    print $ evalFile (splitOn "" (lines file))
evalCommand _ = print "Non-valid command"


evalFile :: [[String]] -> Maybe Term
evalFile [rs, [t]] = do
    term <- stringToTerm t
    trs <- stringsToTRS rs
    return $ evalTerm trs term
evalFile _ = Nothing

readTRS :: IO [Rule]
readTRS = do
    lines <- readLines
    case stringsToTRS lines of
        Nothing -> return []
        Just trs -> return trs

splitOn :: String -> [String] -> [[String]]
splitOn _ []= []
splitOn s l = case break (==s) l of
    (pre, []) -> [pre]
    (xs, a:as) -> xs : splitOn s as

readLines :: IO [String]
readLines = do
    l <- getLine 
    case l of 
        [] -> return []
        _ -> do 
            ls <- readLines
            return (l:ls)