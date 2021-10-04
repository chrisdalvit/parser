import TermParser ( stringToTerm, Term, stringToRule )
import Data.Maybe ( fromMaybe )
import Distribution.Fields (SectionArg(SecArgStr))

-- |Substitutes variables with values
subst :: String -> Int
subst "x" = 12
subst "y" = 3
subst _ = error "Non exhaustiv substitution"

semantic :: String -> ([Int] -> Int)
semantic "+" = sum
semantic "*" = product
semantic _ = error "No exhaustiv semantic"

main :: IO()
main = do
    ln <- readFile "trs.txt"
    print $ map stringToRule $ lines ln