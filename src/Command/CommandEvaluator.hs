module Command.CommandEvaluator (evalCommand) where

import Term.Term (Term, Substitution, Rule)
import Term.TermParser ( stringToTerm, stringsToTRS )
import Term.TermEvaluator  (evalTerm)
import Command.CommandParser(Command(..), CommandSymbol (CommandSymbol), Args (Args), stringToCommand)
import Utils.Assignment (stringToAssignment, findAssignment, addAssignment, Assignment(..), Assignable(..))
import Checker.PatternDisjointness (isPatternDisjoint)
import Checker.LPO (isLPOTerminating)
import Utils.Precedence (argsToPrecedence, Precedence(..))
import Text.Read (prec)
import System.Exit (exitSuccess)

commandName :: Command -> String
commandName (Command (CommandSymbol sym) _) = sym

args :: Command -> Args
args (Command _ args) = args

evalCommand :: [Assignment] -> Command -> IO [Assignment]
evalCommand as cmd
    | name == "term" = evalTermCommand as cmdArgs
    | name == "trs" = evalTRSCommand as cmdArgs
    | name == "trsfile" = readTRSFile as cmdArgs
    | name == "lpo" = evalLPO as cmdArgs
    | name == "pred" = evalPrecedenceCommand as cmdArgs
    | name == "pd" = evalPatternDisjointness as cmdArgs
    | name == "evalfile" = do
        evalFileCommand cmdArgs
        return as
    | name == "printvars" = do
        mapM_ print as
        return as
    | name == "q" = exitSuccess
    | otherwise = do
        putStrLn " -- Non-valid command -- "
        return as
    where
        name = commandName cmd
        cmdArgs = args cmd

evalPrecedenceCommand :: [Assignment] -> Args -> IO [Assignment]
evalPrecedenceCommand as (Args (n:a:args)) = do
    case argsToPrecedence (a:args) of
      Nothing -> do
          putStrLn " -- Error: parsing precedence -- ″"
          return as
      Just prece -> do
        return $ addAssignment (Assignment n (Precedence prece)) as
evalPrecedenceCommand as _ = do
    putStrLn " -- Usage -- "
    putStrLn " :pred  PRED_NAME FSYM>FSYM FSYM>FSYM ..."
    putStrLn " Attention: User is responsible, that entered precedence is valid! \n"
    return as

evalLPO :: [Assignment] -> Args -> IO [Assignment]
evalLPO as (Args [trs, pred]) = do
    case findAssignment trs as of
        Nothing -> putStrLn $ " -- Error: no trs for " ++ trs ++ " -- ″"
        Just (TRS trs) -> case findAssignment pred as of
            Nothing -> putStrLn $ " -- Error: no precedence for " ++ pred ++ " -- ″"
            Just (Precedence p) -> print $ isLPOTerminating trs p
            Just _ -> putStrLn " -- Error: provide precedence -- "
        Just _ -> putStrLn " -- Error: provide trs -- "
    return as
evalLPO as _ = do
    putStrLn " -- Usage -- "
    putStrLn " :lpo  TRS_NAME PRED_NAME \n"
    return as

evalPatternDisjointness :: [Assignment] -> Args -> IO [Assignment]
evalPatternDisjointness as (Args [x]) = do
    case findAssignment x as of
        Nothing -> putStrLn $ " -- Error: no trs for " ++ x ++ " -- ″"
        Just (TRS trs) -> print $ isPatternDisjoint trs
        Just _ -> putStrLn " -- Error: provide trs -- "
    return as
evalPatternDisjointness as _  = do
    putStrLn " -- Usage -- "
    putStrLn " :pd  TRS_NAME \n"
    return as

readTRSFile :: [Assignment] -> Args -> IO [Assignment]
readTRSFile as (Args [f, n]) = do
    file <- readFile f
    case stringsToTRS $ lines file of
        Nothing -> do
            putStrLn " -- Error: incorrect TRS file -- "
            return as
        Just trs -> do
            return $ addAssignment (Assignment n (TRS trs)) as
readTRSFile as _ = do
        putStrLn " -- Usage -- "
        putStrLn " :trsfile  FILE_NAME TRS_NAME \n"
        return as

evalTermCommand :: [Assignment] -> Args -> IO [Assignment]
evalTermCommand as (Args [n, t]) = case stringToTerm t of
    Nothing -> do
        putStrLn " -- No valid term -- "
        return as
    Just t -> do
        print t
        return $ addAssignment (Assignment n (Term t)) as
evalTermCommand as _ = do
        putStrLn " -- Usage -- "
        putStrLn " :term VAR_NAME TERM \n"
        return as

evalTRSCommand :: [Assignment] -> Args -> IO [Assignment]
evalTRSCommand as (Args [n]) = do
    trs <- readTRS
    case trs of
        [] -> putStrLn " -- Non-valid TRS -- "
        _ -> putStrLn $ "TRS: " ++ show trs
    return $ addAssignment (Assignment n (TRS trs)) as
evalTRSCommand as _ = do
        putStrLn " -- Usage -- "
        putStrLn " :trs RULE1 >>ENTER<< RULE2 ... \n"
        return as

evalFileCommand :: Args -> IO()
evalFileCommand (Args [fn]) = do
    file <- readFile fn
    case evalFile (splitOn "" (lines file)) of
        Nothing -> putStrLn " -- Error in file evaluation -- "
        Just res -> print res
evalFileCommand _ = do
    putStrLn " -- Usage -- "
    putStrLn " :evalfile FILE_NAME\n"

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