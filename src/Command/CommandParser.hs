module Command.CommandParser (Command(..), CommandSymbol(..), Args(..), stringToCommand) where

import Parser (Parser(..), char, spaces, parse, split)

newtype CommandSymbol = CommandSymbol String deriving Show
newtype Args = Args [String] deriving Show
data Command = Command CommandSymbol Args deriving Show

command :: Parser Command
command = do
    spaces
    char ':'
    (sym, args) <- split ' '
    return $ Command (CommandSymbol sym) (Args (words args))

stringToCommand :: String -> Maybe Command
stringToCommand s = case parse command s of
    Nothing -> Nothing
    Just (cmd, []) -> return cmd
    Just (cmd, x:xs) -> Nothing