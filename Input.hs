module Input where

import CommandParser (Command(..), command )
import Parser(Parser(..), char, string, sep, token, many1, item, space, spaces, parse, items, many, sat, argWords, parseUntil, split)
import Control.Applicative ((<|>))
import Term (Term, Rule)
import TermParser (parseTerm)
import Data.Char (isAlphaNum)

data Assignable = Term Term | TRS [Rule] deriving Eq
data Assignment = Assignment String Assignable

instance Eq Assignment where
    (Assignment r _) == (Assignment r' _) = r == r'

instance Show Assignable where
    show (Term t) = show t
    show (TRS rs) = unwords $ map show rs

instance Show Assignment where
    show (Assignment x y) = x ++ "=" ++ show y 

parseAssignment :: Parser Assignment
parseAssignment = do
    spaces
    c <- many1 (sat isAlphaNum)
    spaces
    char '='
    spaces
    Assignment c . Term <$> parseTerm

stringToAssignment :: String -> Maybe Assignment
stringToAssignment s = case parse parseAssignment s of
    Nothing -> Nothing
    Just (a, []) -> return a
    Just (a, _) -> Nothing