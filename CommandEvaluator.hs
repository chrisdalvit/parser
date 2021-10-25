module CommandEvaluator (evalCommand) where

import Term (Term, Substitution, Rule)
import TermParser ( stringToTerm, stringsToTRS )
import TermEvaluator  (evalTerm)
import CommandParser(Command(..), CommandSymbol (CommandSymbol), Args (Args), stringToCommand)
import Input (stringToAssignment, Assignment(..), Assignable(..))
import Data.List (delete)

evalCommand :: [Assignment] -> Command -> IO [Assignment]
evalCommand as (Command (CommandSymbol "term") args) = evalTermCommand as args
evalCommand as (Command (CommandSymbol "trs") _ ) = do 
    evalTRSCommand
    return as
evalCommand as (Command (CommandSymbol "file") args) = do
    evalFileCommand args
    return as
evalCommand as (Command (CommandSymbol "=") args) = evalAssignmentCommand as args
evalCommand as (Command (CommandSymbol "p") args) = do
    print as
    return as
evalCommand as _ = do
    print "Non-valid command"
    return as


addAssignment :: Assignment -> [Assignment] -> [Assignment]
addAssignment a as
    | a `elem` as = a:as'
    | otherwise = a:as
    where
        as' = delete a as

evalAssignmentCommand :: [Assignment] -> Args -> IO [Assignment]
evalAssignmentCommand as (Args [x]) = do
    case stringToAssignment x of
        Nothing -> do
            print "Error when parsing assignment"
            return as
        Just a -> do 
            print a
            return $ addAssignment a as
evalAssignmentCommand as _ = do 
    print "Wrong number of arguments"
    return as

evalTermCommand :: [Assignment] -> Args -> IO [Assignment]
evalTermCommand as (Args [x]) = do
    print $ stringToTerm x
    return as
evalTermCommand as (Args [n, t]) = case stringToTerm t of
    Nothing -> do 
        print "No valid term"
        return as
    Just t -> do
        print t
        return $ addAssignment (Assignment n (Term t)) as
evalTermCommand as _ = do 
    print "Error: not enough arguments"
    return as

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