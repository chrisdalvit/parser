module Term (Term(Func, Var), Rule(Rule)) where

data Term = Func String [Term] | Var String deriving (Show, Eq)
data Rule = Rule Term Term deriving Show

vars :: Term -> [String]
vars (Var a) = [a]
vars (Func _ ts) = concatMap vars ts