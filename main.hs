import TermParser ( stringToTerm, stringsToTRS )
import TermEvaluator  (evalTerm)
import Term (Term, Substitution, Rule)

import Data.Maybe (mapMaybe)
import CommandParser(Command(..), CommandSymbol (CommandSymbol), Args (Args), stringToCommand)

splitOn :: String -> [String] -> [[String]]
splitOn _ []= []
splitOn s l = case break (==s) l of
    (pre, []) -> [pre]
    (xs, a:as) -> xs : splitOn s as

evalFile :: [[String]] -> Maybe Term
evalFile [rs, [t]] = do
    term <- stringToTerm t
    trs <- stringsToTRS rs
    return $ evalTerm trs term
evalFile _ = Nothing

evalCommand :: Command -> IO ()
evalCommand (Command (CommandSymbol "term") (Args xs)) = do
    print $ map stringToTerm xs
evalCommand (Command (CommandSymbol "trs") _ ) = do
    trs <- readTRS
    case trs of 
        [] -> print "Non-valid TRS"
        _ -> print trs
evalCommand _ = print "Non-valid command"

readLines :: IO [String]
readLines = do
    l <- getLine 
    case l of 
        [] -> return []
        _ -> do 
            ls <- readLines
            return (l:ls)

readTRS :: IO [Rule]
readTRS = do
    lines <- readLines
    case stringsToTRS lines of
        Nothing -> return []
        Just trs -> return trs

main :: IO()
main = do
    inp <- getLine
    case stringToCommand inp of
        Nothing -> putStr ""
        Just cmd -> evalCommand cmd
    main