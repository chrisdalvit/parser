module CommandParser where

import Parser (Parser, char, string, sep, token, many1, item, space, spaces, parse, items)

newtype CommandSymbol = CommandSymbol String deriving Show
newtype Args = Args [String] deriving Show
data Command = Command CommandSymbol Args deriving Show

commandSymb :: Parser CommandSymbol
commandSymb = do
    char ':'
    CommandSymbol <$> items

args :: Parser Args
args = do
    as <- sep (many1 item) spaces
    return $ Args (words $ concat as)

command :: Parser Command
command = do
    cmd <- commandSymb
    Command cmd <$> args