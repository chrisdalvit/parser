module Input where

import Parser(Parser, char, many1, parse, sat, token)
import Term (Term, Rule)
import TermParser (parseTerm)
import Data.Char (isAlphaNum)
import Data.List (delete)

data Assignable = Term Term | TRS [Rule] deriving (Eq, Show)
data Assignment = Assignment String Assignable

instance Eq Assignment where
    (Assignment r _) == (Assignment r' _) = r == r'

instance Show Assignment where
    show (Assignment x y) = x ++ "=" ++ show y 

parseAssignment :: Parser Assignment
parseAssignment = do
    c <- token $ many1 (sat isAlphaNum)
    char '='
    term <- token parseTerm
    return $ Assignment c (Term term)

addAssignment :: Assignment -> [Assignment] -> [Assignment]
addAssignment a as
    | a `elem` as = a:as'
    | otherwise = a:as
    where
        as' = delete a as

stringToAssignment :: String -> Maybe Assignment
stringToAssignment s = case parse parseAssignment s of
    Nothing -> Nothing
    Just (a, []) -> return a
    Just (a, _) -> Nothing