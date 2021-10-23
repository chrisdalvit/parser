module CommandEvaluator (evalCommand) where

import Term (Term, Substitution, Rule)
import TermParser ( stringToTerm, stringsToTRS )
import TermEvaluator  (evalTerm)
import CommandParser(Command(..), CommandSymbol (CommandSymbol), Args (Args), stringToCommand)
import Input (stringToAssignment, Expression (..))

evalCommand :: [(String, String)] -> Command -> (IO (), [(String, String)])
evalCommand as (Command (CommandSymbol "term") args) = (evalTermCommand args, as)
evalCommand as (Command (CommandSymbol "trs") _ ) = (evalTRSCommand, as) 
evalCommand as (Command (CommandSymbol "file") args) = (evalFileCommand args, as)
evalCommand as (Command (CommandSymbol "=") args) = evalAssignmentCommand as args
evalCommand as _ = (print "Non-valid command", as)

evalAssignmentCommand :: [(String, String)] -> Args -> (IO(), [(String, String)])
evalAssignmentCommand as (Args [x]) = do
    case stringToAssignment x of
        Nothing -> (print "Error when parsing assignment", as)
        Just a@(Assignment lhs rhs) -> (print a, (lhs, rhs):as)
evalAssignmentCommand as _ = (print "Wrong number of arguments", as)

evalTermCommand :: Args -> IO()
evalTermCommand (Args [x]) = do
    print $ map stringToTerm [x]
evalTermCommand _ = print "Error: not enough arguments"

evalTRSCommand :: IO()
evalTRSCommand = do
    trs <- readTRS
    case trs of 
        [] -> print "Non-valid TRS"
        _ -> print trs

evalFileCommand :: Args -> IO()
evalFileCommand (Args [fn]) = do
    file <- readFile fn
    print $ evalFile (splitOn "" (lines file))
evalFileCommand _ = print "Error: too many or to few arguments"

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