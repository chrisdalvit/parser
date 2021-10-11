module TermParser (Term, Rule, stringsToTRS, stringToTerm) where

import Parser (Parser, parse, char, item , string, comma, sep, sat, token)
import Term ( Rule(..), Term(..), vars, subset, funcArity)
import Control.Applicative (Alternative ((<|>)))
import Data.Char (isAsciiLower)
import Data.Maybe (mapMaybe)
import Data.String (String)
import Control.Monad.STM (check)


-- |Term parser, for either parsing a function or a variable
parseTerm :: Parser Term
parseTerm = parseFunc <|> parseVar

-- |Parser for parsing a term variable, variables are assumed to be ASCII lower-case characters
parseVar :: Parser Term
parseVar = do
    v <- token $ sat isAsciiLower
    return (Var [v])

-- |Parser for parsing a term function
parseFunc :: Parser Term
parseFunc = do
    f <- token item
    token $ char '('
    ts <- token $ sep parseTerm comma
    token $ char ')'
    return (Func [f] ts)

-- |Parser for parsing termrewrite rules
parseRule :: Parser Rule
parseRule = do
    lhs <- token parseFunc
    token $ string "->"
    rhs <- token parseTerm
    return $ Rule lhs rhs

-- |Function that trys to parse a given string into a rewriterule
stringToRule :: String -> Maybe Rule
stringToRule s = case parse parseRule s of
    Nothing -> Nothing
    Just (Rule lhs rhs, []) -> if subset (vars rhs) (vars lhs) then return $ Rule lhs rhs else Nothing
    Just (_ , x:xs) -> Nothing

-- |Function that trys to parse a given string into a term
stringToTerm :: String -> Maybe Term
stringToTerm s = case parse parseTerm s of
    Nothing -> Nothing
    Just(x, c:cs) -> Nothing
    Just(x, []) -> return x

-- |Funtcion that takes a list of strings and returns a list of rules if all strings can be parsed into rules
parseTRS :: [String] -> Maybe [Rule]
parseTRS [] = return []
parseTRS (x:xs) = do
    r <- stringToRule x
    rs <- parseTRS xs
    return (r:rs)

stringsToTRS :: [String] -> Maybe [Rule]
stringsToTRS l = case parseTRS l of
    Nothing -> Nothing
    Just rs -> if validTRS rs then return rs else Nothing

validTRS :: [Rule] -> Bool
validTRS [] = True
validTRS rs = checkArity arities
    where
        arities = concatMap (\(Rule l r) -> funcArity l ++ funcArity r) rs

checkArity :: [(String, Int)] -> Bool
checkArity [] = True
checkArity ((f,a): xs) = case lookup f xs of
    Nothing -> checkArity xs
    Just a' -> a == a' && checkArity xs
    