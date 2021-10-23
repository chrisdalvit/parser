import Data.Maybe (mapMaybe)
import CommandEvaluator (evalCommand)
import CommandParser ( stringToCommand )

main :: IO()
main = evalLoop []

evalLoop :: [(String, String)] -> IO()
evalLoop as = do
    inp <- getLine
    as' <- evalInput inp as
    evalLoop as'

evalInput :: String -> [(String, String)] -> IO[(String, String)]
evalInput inp as =
    case stringToCommand inp of
        Nothing -> return as
        Just cmd -> do
            fst $ evalCommand as cmd
            return $ snd $ evalCommand as cmd
