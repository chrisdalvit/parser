module Input where

import CommandParser (Command(..), command )
import Parser(Parser(..), char, string, sep, token, many1, item, space, spaces, parse, items, many, sat, argWords, parseUntil, split)
import Control.Applicative ((<|>))

data Expression = Assignment String String deriving Show
data Input = CmdInput Command | ExpInput Expression deriving Show

parseExpression :: Parser Input
parseExpression = undefined

parseAssignment :: Parser Expression
parseAssignment = do
    (l,r) <- split '='
    return $ Assignment l r

parseCommand :: Parser Input
parseCommand = do
    CmdInput <$> command

parseInput :: Parser Input
parseInput = parseExpression <|> parseCommand

stringToAssignment :: String -> Maybe Expression
stringToAssignment s = case parse parseAssignment s of 
    Nothing -> Nothing 
    Just (a, []) -> return a
    Just (a, _) -> Nothing