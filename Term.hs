module Term (Term(Func, Var), Rule(Rule), Substitution(..)) where

data Term = Func String [Term] | Var String deriving Eq

instance Show Term where
    show (Var x) = x
    show (Func f args) = f ++ "(" ++ concatMap show args ++ ")"

data Rule = Rule Term Term
instance Show Rule where
    show (Rule rhs lhs) = show rhs ++ " = " ++ show lhs

newtype Substitution = Substitution [(String, Term)]
instance Show Substitution where
    show (Substitution []) = ""
    show (Substitution ((var, t):xs) )= var ++ " -> " ++ show t ++ "\n" ++ show (Substitution xs)

vars :: Term -> [String]
vars (Var a) = [a]
vars (Func _ ts) = concatMap vars ts