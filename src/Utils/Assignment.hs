module Utils.Assignment where

import Utils.Parser(Parser, char, many1, parse, sat, token)
import Utils.Precedence (Precedence(..))
import Term.Term (Term, Rule)
import Term.TermParser (parseTerm)
import Data.Char (isAlphaNum)
import Data.List (delete)

data Assignable = Term Term | TRS [Rule] | Precedence Precedence deriving Eq
data Assignment = Assignment String Assignable

instance Show Assignable where
    show (Term t) = show "Term: " ++ show t
    show (TRS trs) = "TRS: [" ++ concatMap (\r -> "\n\t" ++ show r ++ ", ") trs  ++ "\n]"
    show (Precedence p) = show p

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

findAssignment :: String -> [Assignment] -> Maybe Assignable
findAssignment s [] = Nothing 
findAssignment s (Assignment l r: xs)
    | s == l = return r
    | otherwise = findAssignment s xs

stringToAssignment :: String -> Maybe Assignment
stringToAssignment s = case parse parseAssignment s of
    Nothing -> Nothing
    Just (a, []) -> return a
    Just (a, _) -> Nothing