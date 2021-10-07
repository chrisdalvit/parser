import TermParser ( stringToTerm, parseTRS )
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
    trs <- parseTRS rs
    return $ evalTerm trs term
evalFile _ = Nothing

evalCommand :: Command -> IO ()
evalCommand (Command (CommandSymbol "term") (Args xs)) = do
    print $ map stringToTerm xs
evalCommand (Command (CommandSymbol "trs") _ ) = do
    trs <- readTRS
    print trs
evalCommand _ = print "Non-valid command"

readTRS :: IO [Rule]
readTRS = do
    line <- getLine
    case parseTRS [line] of
        Nothing -> return []
        Just trs -> do
            trs' <- readTRS
            return $ trs ++ trs'

main :: IO()
main = do
    inp <- getLine
    case stringToCommand inp of
        Nothing -> print "Error"
        Just cmd -> evalCommand cmd
    main