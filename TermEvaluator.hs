module TermEvaluator (matchRule, singleEvalStep) where

import Term ( Rule (..), Term(..),  Substitution(..))
import Data.Maybe (mapMaybe, fromMaybe)

applySubst :: Substitution -> Term -> Term
applySubst (Substitution sub) v@(Var x) = fromMaybe v $ lookup x sub
applySubst s@(Substitution sub) f@(Func fsymb args) = Func fsymb args'
    where
        args' = map (applySubst s) args

singleEvalStep :: [Rule] -> Term -> Maybe Term
singleEvalStep [] t = return t
singleEvalStep rs t = do
    (Rule rhs lhs, sub) <- matchRule rs t
    return $ applySubst sub lhs

matchRule :: [Rule] -> Term -> Maybe (Rule, Substitution)
matchRule [] _ = Nothing
matchRule (r@(Rule lhs _):rs) t = case match [] [(lhs,t)] of
    Nothing -> matchRule rs t
    Just s -> return (r, Substitution s)

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
