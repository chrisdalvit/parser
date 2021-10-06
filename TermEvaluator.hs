module TermEvaluator (matchRule, evalTerm) where

import Term ( Rule (..), Term(..),  Substitution(..))
import Data.Maybe (mapMaybe, fromMaybe)

applySubst :: Substitution -> Term -> Term
applySubst (Substitution sub) v@(Var x) = fromMaybe v $ lookup x sub
applySubst s@(Substitution sub) f@(Func fsymb args) = Func fsymb args'
    where
        args' = map (applySubst s) args

rootStep :: [Rule] -> Term -> Maybe Term
rootStep rs (Var x) = return $ Var x
rootStep rs t@(Func f ts) = case matchRule rs t of
    Just (Rule _ rhs, sub) -> return $ applySubst sub rhs
    Nothing -> return t

topDownStep :: [Rule] -> Term -> Term
topDownStep rs (Var x) = Var x
topDownStep rs t@(Func f ts) = case rootStep rs t of
    Just (Func f ts') -> Func f (map (topDownStep rs) ts')
    Just (Var x) -> Var x
    Nothing -> Func f (map (topDownStep rs) ts)

evalTerm :: [Rule] -> Term -> Term
evalTerm rs t
    | t' == t = t
    | otherwise = topDownStep rs t'
    where
        t' = topDownStep rs t

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
