module TermEvaluator (matchRule) where

import Term ( Rule (..), Term(..))
import Data.Maybe (mapMaybe)

matchRule :: [Rule] -> Term -> Maybe [(String, Term)]
matchRule [] _ = Nothing 
matchRule ((Rule lhs _):rs) t = case match [] [(lhs,t)] of
    Nothing -> matchRule rs t 
    Just s -> return s

match :: [(String, Term)] -> [(Term,Term)] -> Maybe [(String,  Term)]
match [] [] = Nothing 
match s [] = return s
match _ ( (Func _ _, Var _) : _) = Nothing    
match s ( (Func f as, Func g bs) : xs )
    | f == g && length as == length bs = match s (xs ++ zip as bs)
    | otherwise = Nothing 
match s ( (Var x, Var t) : xs ) = case lookup x s of
    Nothing -> match ((x, Var t):s) xs
    Just (Var t') -> if t == t' then match s xs else Nothing
    Just (Func _ _) -> Nothing 
match s ( (Var x, f) : xs) = case lookup x s of
    Nothing -> match ((x, f):s) xs
    Just f'@(Func _ _) -> if f == f' then match s xs else Nothing  
    Just (Var x') -> Nothing 
