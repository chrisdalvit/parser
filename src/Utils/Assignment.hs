module Utils.Assignment where

import Utils.Parser(Parser, char, many1, parse, sat, token)
import Utils.Precedence (Precedence(..))
import Term.Term (Term, Rule)
import Data.Char (isAlphaNum)
import Data.List (delete)

data Assignable = Term Term | TRS [Rule] | Precedence Precedence deriving Eq
data Assignment = Assignment String Assignable

instance Show Assignable where
    show (Term t) = "Term: " ++ show t
    show (TRS trs) = "TRS: [" ++ concatMap (\r -> "\n\t" ++ show r ++ ", ") trs  ++ "\n]"
    show (Precedence p) = show p

instance Eq Assignment where
    (Assignment r _) == (Assignment r' _) = r == r'

instance Show Assignment where
    show (Assignment x y) = x ++ " = " ++ show y 

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
