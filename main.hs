import Data.Maybe (mapMaybe)
import CommandEvaluator (evalCommand)
import CommandParser ( stringToCommand )

main :: IO()
main = do
    inp <- getLine
    case stringToCommand inp of
        Nothing -> putStr ""
        Just cmd -> evalCommand cmd
    main