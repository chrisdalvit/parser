module CommandParser (Command(..), CommandSymbol(..), Args(..), stringToCommand, command) where

import Parser (Parser(..), char, string, sep, token, many1, item, space, spaces, parse, items, many, sat, argWords, split)
import GHC.Unicode (isAlphaNum, isPrint)

newtype CommandSymbol = CommandSymbol String deriving Show
newtype Args = Args [String] deriving Show
data Command = Command CommandSymbol Args deriving Show

commandSymb :: Parser CommandSymbol
commandSymb = do
    spaces
    char ':'
    (sym, _) <- split ' '
    return $ CommandSymbol sym

args :: Parser Args
args = do
    Args <$> argWords

command :: Parser Command
command = do
    cmd <- commandSymb
    Command cmd <$> args

stringToCommand :: String -> Maybe Command
stringToCommand s = case parse command s of
    Nothing -> Nothing
    Just (cmd, []) -> return cmd
    Just (cmd, x:xs) -> Nothing