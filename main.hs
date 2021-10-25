import Data.Maybe (mapMaybe)
import CommandEvaluator (evalCommand)
import CommandParser ( stringToCommand )
import Input ( Assignment(..) )

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