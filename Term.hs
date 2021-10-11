module Term (Term(Func, Var), Rule(Rule), Substitution(..), vars, subset, funcArity) where
import Data.Tree (Tree(subForest))

data Term = Func String [Term] | Var String deriving Eq
data Rule = Rule Term Term
newtype Substitution = Substitution [(String, Term)]

instance Show Term where
    show (Var x) = x
    show (Func f args) = f ++ "(" ++ printArgs args ++ ")"

printArgs :: [Term] -> String
printArgs [] = ""
printArgs [x] = show x
printArgs [x, y] = show x ++ "," ++ show y
printArgs (x:xs) = show x ++ "," ++ printArgs xs

instance Show Rule where
    show (Rule rhs lhs) = show rhs ++ " -> " ++ show lhs

instance Show Substitution where
    show (Substitution []) = ""
    show (Substitution ((var, t):xs) )= var ++ " -> " ++ show t ++ "\n" ++ show (Substitution xs)

vars :: Term -> [String]
vars (Var a) = [a]
vars (Func _ ts) = concatMap vars ts

funcArity :: Term -> [(String, Int)]
funcArity (Var x) = []
funcArity (Func f ts) = (f, length ts) : concatMap funcArity ts 

subset :: [String] -> [String] -> Bool
subset [] _ = True 
subset (x:xs) l = elem x l && subset xs l