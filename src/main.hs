import Data.Maybe (mapMaybe)
import Command.CommandEvaluator (evalCommand)
import Command.CommandParser ( stringToCommand )
import Assignment ( Assignment(..) )

main :: IO()
main = evalLoop []

evalLoop :: [Assignment] -> IO()
evalLoop as = do
    inp <- getLine
    as' <- evalInput inp as
    evalLoop as'

evalInput :: String -> [Assignment] -> IO[Assignment]
evalInput inp as =
    case stringToCommand inp of
        Nothing -> do
            printAssignment inp as
            return as
        Just cmd -> evalCommand as cmd


printAssignment :: String -> [Assignment] -> IO()
printAssignment s [] = return ()
printAssignment s (Assignment r l : as)
    | s == r = print l
    | otherwise = printAssignment s as