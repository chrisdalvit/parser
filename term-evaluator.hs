import TermParser ( stringToTerm, Term, vars )
import Data.Maybe ( fromMaybe )
import Data.Tree (Tree(subForest))

subst :: String -> Int
subst "x" = 12
subst "y" = 3
subst _ = error "No valid substitution"


main :: IO()
main = do
    ln <- getLine
    print $ do
        term <- stringToTerm ln
        return (subst <$> term)