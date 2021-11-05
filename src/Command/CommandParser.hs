module Command.CommandParser (Command(..), CommandSymbol(..), Args(..), stringToCommand) where

import Utils.Parser
import GHC.Unicode (isPrint)
import Data.Char (isSpace)
import Control.Applicative ( Alternative((<|>)) )

newtype CommandSymbol = CommandSymbol String deriving Show
newtype Args = Args [String] deriving Show
data Command = Command CommandSymbol Args deriving Show

command :: Parser Command
command = commandWithArgs <|> commandWithoutArgs

commandWithArgs :: Parser Command
commandWithArgs = do
    spaces
    char ':'
    (sym, args) <- split ' '
    return $ Command (CommandSymbol sym) (Args (words args))

commandWithoutArgs :: Parser Command
commandWithoutArgs = do
    spaces
    char ':'
    sym <- many1 (sat (\c -> isPrint c && not (isSpace c)))
    return $ Command (CommandSymbol sym) (Args [])


stringToCommand :: String -> Maybe Command
stringToCommand s = case parse command s of
    Nothing -> Nothing
    Just (cmd, []) -> return cmd
    Just (cmd, x:xs) -> Nothing