module CommandEvaluator (evalCommand) where

import Term (Term, Substitution, Rule)
import TermParser ( stringToTerm, stringsToTRS )
import TermEvaluator  (evalTerm)
import CommandParser(Command(..), CommandSymbol (CommandSymbol), Args (Args), stringToCommand)
import Input (stringToAssignment, Assignment(..), Assignable(..))
import Data.List (delete)

evalCommand :: [Assignment] -> Command -> (IO (), [Assignment])
evalCommand as (Command (CommandSymbol "term") args) = evalTermCommand as args
evalCommand as (Command (CommandSymbol "trs") _ ) = (evalTRSCommand, as)
evalCommand as (Command (CommandSymbol "file") args) = (evalFileCommand args, as)
evalCommand as (Command (CommandSymbol "=") args) = evalAssignmentCommand as args
evalCommand as (Command (CommandSymbol "p") args) = (print as, as)
evalCommand as _ = (print "Non-valid command", as)


addAssignment :: Assignment -> [Assignment] -> [Assignment]
addAssignment a as
    | a `elem` as = a:as'
    | otherwise = a:as
    where
        as' = delete a as

evalAssignmentCommand :: [Assignment] -> Args -> (IO(), [Assignment])
evalAssignmentCommand as (Args [x]) = do
    case stringToAssignment x of
        Nothing -> (print "Error when parsing assignment", as)
        Just a -> (print a, addAssignment a as)
evalAssignmentCommand as _ = (print "Wrong number of arguments", as)

evalTermCommand :: [Assignment] -> Args -> (IO(), [Assignment])
evalTermCommand as (Args [x]) = (print $ stringToTerm x, as)
evalTermCommand as (Args [n, t]) = case stringToTerm t of 
    Nothing -> (print "No valid term", as)
    Just t -> (print t, addAssignment (Assignment n (Term t)) as)
evalTermCommand as _ = (print "Error: not enough arguments", as)

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