module Checker.PatternDisjointness where
import Term.Term (Rule(..), Term(..), vars, Substitution(..))

import Term.TermParser ( stringToTerm, stringsToTRS )
import Term.TermEvaluator (applySubst)

isPatternDisjoint :: [Rule] -> Bool
isPatternDisjoint = applyUnification .lhsCombinations . fst . renameTerms 0 . lhs

lhs :: [Rule] -> [Term]
lhs = map (\(Rule l r) -> l)

lhsCombinations :: [Term] -> [(Term, Term)]
lhsCombinations ts = [ (s,t) | s <- ts, t <- ts, s /= t]

applyUnification :: [(Term, Term)] -> Bool
applyUnification = all (\p -> not $ unify [p])

unify :: [(Term, Term)] -> Bool
unify [] = True
unify ((Var x, Var y):xs)
    | x == y = unify xs
    | otherwise = unify $ map mapSubst xs
    where 
        subst = Substitution [(x, Var y)]
        mapSubst = \(r,l) -> (applySubst subst r, applySubst subst l)
unify ((Func f ts, Func g ts'):xs)
    | f == g && length ts == length ts' = unify $ zip ts ts' ++ xs
    | otherwise = False
unify ((Func f ts, Var x):xs) = unify ((Var x, Func f ts):xs)
unify ((Var x, f@(Func _ _)):xs)
    | x `elem` vars f = False
    | otherwise = unify $ map mapSubst xs
    where
        subst = Substitution [(x, f)]
        mapSubst = \(r,l) -> (applySubst subst r, applySubst subst l)


renameTerm :: Term -> Int -> (Term, Int)
renameTerm (Var x) i = (Var $ "x" ++ show i, i+1)
renameTerm (Func f ts) i = (Func f ts', i')
    where
        (ts', i') = renameTerms i ts

renameTerms :: Int -> [Term] -> ([Term], Int)
renameTerms i [] = ([], i)
renameTerms i (x:xs) = (t' : ts', i'')
    where
        (t', i') = renameTerm x i
        (ts', i'') = renameTerms i' xs
