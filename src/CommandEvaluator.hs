module CommandEvaluator (evalCommand) where

import Term.Term (Term, Substitution, Rule)
import Term.TermParser ( stringToTerm, stringsToTRS )
import Term.TermEvaluator  (evalTerm)
import CommandParser(Command(..), CommandSymbol (CommandSymbol), Args (Args), stringToCommand)
import Input (stringToAssignment, addAssignment, Assignment(..), Assignable(..))

evalCommand :: [Assignment] -> Command -> IO [Assignment]
evalCommand as (Command (CommandSymbol "term") args) = evalTermCommand as args
evalCommand as (Command (CommandSymbol "trs") args ) = evalTRSCommand as args
evalCommand as (Command (CommandSymbol "trsfile") args ) = readTRSFile as args
evalCommand as (Command (CommandSymbol "evalfile") args) = do
    evalFileCommand args
    return as
evalCommand as (Command (CommandSymbol "=") args) = evalAssignmentCommand as args
evalCommand as (Command (CommandSymbol "p") args) = do
    print as
    return as
evalCommand as _ = do
    putStrLn " -- Non-valid command -- "
    return as

readTRSFile :: [Assignment] -> Args -> IO [Assignment]
readTRSFile as (Args [f]) = do
    file <- readFile f
    print $ stringsToTRS $ lines file
    return as
readTRSFile as (Args [f, n]) = do
    file <- readFile f
    case stringsToTRS $ lines file of
        Nothing -> do
            putStrLn " -- Error: incorrect TRS -- "
            return as 
        Just trs -> do
            return $ addAssignment (Assignment n (TRS trs)) as
readTRSFile as _ = do
    putStrLn " -- Error: wrong number of arguments -- "
    return as

evalAssignmentCommand :: [Assignment] -> Args -> IO [Assignment]
evalAssignmentCommand as (Args [x]) = do
    case stringToAssignment x of
        Nothing -> do
            putStrLn " -- Error when parsing assignment -- "
            return as
        Just a -> do 
            print a
            return $ addAssignment a as
evalAssignmentCommand as _ = do 
    putStrLn " -- Wrong number of arguments -- "
    return as

evalTermCommand :: [Assignment] -> Args -> IO [Assignment]
evalTermCommand as (Args [x]) = do
    print $ stringToTerm x
    return as
evalTermCommand as (Args [n, t]) = case stringToTerm t of
    Nothing -> do 
        putStrLn " -- No valid term -- "
        return as
    Just t -> do
        print t
        return $ addAssignment (Assignment n (Term t)) as
evalTermCommand as _ = do 
    putStrLn " -- Error: not enough arguments -- "
    return as

evalTRSCommand :: [Assignment] -> Args -> IO [Assignment]
evalTRSCommand as (Args []) = do
    trs <- readTRS
    case trs of
        [] -> putStrLn " -- Non-valid TRS -- "
        _ -> putStrLn $ "TRS: " ++ show trs
    return as
evalTRSCommand as (Args [n]) = do
    trs <- readTRS
    case trs of
        [] -> putStrLn " -- Non-valid TRS -- "
        _ -> putStrLn $ "TRS: " ++ show trs
    return $ addAssignment (Assignment n (TRS trs)) as
evalTRSCommand as _ = do
    putStrLn " -- Error: wrong number of arguments -- "
    return as

evalFileCommand :: Args -> IO()
evalFileCommand (Args [fn]) = do
    file <- readFile fn
    case evalFile (splitOn "" (lines file)) of
        Nothing -> putStrLn " -- Error in file evaluation -- "
        Just res -> putStrLn $ "Result: " ++ show res
evalFileCommand _ = putStrLn " -- Error: too many or to few arguments -- "

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
        "" -> return []
        _ -> do
            ls <- readLines
            return (l:ls)