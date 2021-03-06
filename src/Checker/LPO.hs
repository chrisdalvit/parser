module Checker.LPO where

import Term.Term (Term(..), Rule(..))
import Utils.Precedence ( Precedence(..) )

-- | Data type for term and a given precedence in a LPO context
data LPO = LPOTerm Term Precedence deriving Eq

-- | Ordering of LPO terms
-- | Let s and t be LPO terms, then s <= t iff they have the same precedence and one of the three cases holds or s == t
-- | LPO  s <= t
lpoLe :: LPO -> LPO -> Bool
lpoLe (LPOTerm s p) (LPOTerm t p') = p == p' &&
                (s == t ||
                firstCase p s t ||
                secondCase p s t ||
                thirdCase p s t)
-- | LPO  s < t
lpoL :: LPO -> LPO -> Bool
lpoL (LPOTerm s p) (LPOTerm t p') = p == p' &&
                (firstCase p s t ||
                secondCase p s t ||
                thirdCase p s t)

-- | LPO  s >= t
lpoGe :: LPO -> LPO -> Bool
lpoGe (LPOTerm s p) (LPOTerm t p') = p == p' &&
                (s == t ||
                firstCase p t s ||
                secondCase p t s ||
                thirdCase p t s)
-- | LPO  s > t
lpoG :: LPO -> LPO -> Bool
lpoG (LPOTerm s p) (LPOTerm t p') = p == p' &&
                (firstCase p t s ||
                secondCase p t s ||
                thirdCase p t s)

-- Checks if t = f(t_1,...,t_n), s = f(s_1,...,s_n) and there exists 1 <= i <= n with
-- s_j = t_j forall 1 <= j < i
-- t_i > s_i  
-- t > s_j forall i < j <= n
firstCase :: Precedence -> Term -> Term -> Bool
firstCase p s@(Func f as) t@(Func g bs)
    | f == g = case dropWhile (uncurry (==)) $ zip as bs of
        [] -> False
        ((a',b'):xs) -> lpoL (LPOTerm a' p) (LPOTerm b' p) && all (lpoG (LPOTerm t p) . (`LPOTerm` p)  . fst) xs
    | otherwise = False
firstCase _ _ _ = False

-- Checks if t = g(t_1,...,t_m), s = f(s_1,...,s_n) and g > f and t > s_i forall 1 <= i <= n
secondCase :: Precedence -> Term -> Term -> Bool
secondCase p@(Pred ps) (Func f as) t@(Func g bs)
    | f /= g = (g,f) `elem` ps && all (lpoG (LPOTerm t p)) as'
    | otherwise = False
    where
        as' = map (`LPOTerm` p) as
secondCase _ _ _ = False

-- Checks if t_i >= s for some t = f(t_1,...,t_n)
thirdCase :: Precedence -> Term -> Term -> Bool
thirdCase p s (Func _ ts) = any (lpoLe (LPOTerm s p)) ts'
    where
        ts' = map (`LPOTerm` p) ts
thirdCase _ _ _ = False

-- Checks if for all rules lhs > rhs with a given precedence, the user is responsible for giving a valid precedence
isLPOTerminating :: [Rule] -> Precedence -> Bool
isLPOTerminating [] _ = True
isLPOTerminating ((Rule lhs rhs):rs) p = lpoG (LPOTerm lhs p) (LPOTerm rhs p) && isLPOTerminating rs p