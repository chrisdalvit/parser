module Utils.Assignment where

import Utils.Parser(Parser, char, many1, parse, sat, token)
import Utils.Precedence (Precedence(..))
import Term.Term (Term, Rule)
import Data.Char (isAlphaNum)
import Data.List (delete)

-- | 'Assignable' are values in an 'Assignment' and can be either a 'Term', 'TRS' or a 'Precedence'
data Assignable = Term Term | TRS [Rule] | Precedence Precedence deriving Eq
-- | 'Assignment' consists of a name of type 'String' and a value of type 'Assignable'
data Assignment = Assignment String Assignable

instance Show Assignable where
    show (Term t) = "Term: " ++ show t
    show (TRS trs) = "TRS: [" ++ concatMap (\r -> "\n\t" ++ show r ++ ", ") trs  ++ "\n]"
    show (Precedence p) = show p

instance Eq Assignment where
    (Assignment r _) == (Assignment r' _) = r == r'

instance Show Assignment where
    show (Assignment x y) = x ++ " = " ++ show y 

-- | Add an assignment to the assignment list and return the updates list.
-- If the assignment already exists in the assignment list, the value will be overwritten.
addAssignment :: Assignment -> [Assignment] -> [Assignment]
addAssignment a as
    | a `elem` as = a:as'
    | otherwise = a:as
    where
        as' = delete a as

-- | Return the value of an assignment, if it exists.
findAssignment :: String -> [Assignment] -> Maybe Assignable
findAssignment s [] = Nothing 
findAssignment s (Assignment l r: xs)
    | s == l = return r
    | otherwise = findAssignment s xs
