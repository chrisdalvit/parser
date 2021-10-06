module CommandParser where

import Parser (Parser, char, string, sep, token, many1, item, space, spaces, parse, items, many, sat)
import GHC.Unicode (isAlphaNum)

newtype CommandSymbol = CommandSymbol String deriving Show
newtype Args = Args [String] deriving Show
data Command = Command CommandSymbol Args deriving Show

commandSymb :: Parser CommandSymbol
commandSymb = do
    char ':'
    sym <- token $ many1 (sat isAlphaNum)
    return $ CommandSymbol sym

args :: Parser Args
args = do
    as <- sep (many1 (sat isAlphaNum)) spaces
    return $ Args as

command :: Parser Command
command = do
    cmd <- commandSymb
    Command cmd <$> args

stringToCommand :: String -> Maybe Command
stringToCommand s = case parse command s of 
    Nothing -> Nothing
    Just (cmd, []) -> return cmd
    Just (cmd, x:xs) -> Nothing