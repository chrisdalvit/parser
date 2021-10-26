module TermParser (Term, Rule, stringsToTRS, stringToTerm, parseTerm) where

import Parser (Parser, parse, char, string, comma, sep, sat, token, many1)
import Term ( Rule(..), Term(..), validTRS, validRule)
import Control.Applicative (Alternative ((<|>)))
import Data.Char (isAsciiLower, isAlphaNum)

-- |Term parser, for either parsing a function or a variable
parseTerm :: Parser Term
parseTerm = parseFunc <|> parseVar

-- |Parser for parsing a term variable, variables are assumed to be ASCII lower-case characters
parseVar :: Parser Term
parseVar = do
    v <- token $ sat isAsciiLower
    return (Var [v])

-- |Parser for parsing a term function
-- TODO: Implement clean function parser
parseFunc :: Parser Term
parseFunc = do
    f <- many1 (sat isAlphaNum)
    char '('
    ts <- sep (token parseTerm) comma
    char ')'
    return (Func f ts)

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
    Just (r, []) -> if validRule r then return r else Nothing
    Just (_ , x:xs) -> Nothing

-- |Function that trys to parse a given string into a term
stringToTerm :: String -> Maybe Term
stringToTerm s = case parse parseTerm s of
    Nothing -> Nothing
    Just(x, c:cs) -> Nothing
    Just(x, []) -> return x

-- |Funtcion that takes a list of strings and returns a list of rules if all strings can be parsed into rules
stringsToTRS :: [String] -> Maybe [Rule]
stringsToTRS [] = return []
stringsToTRS (x:xs) = do
    r <- stringToRule x
    rs <- stringsToTRS xs
    if validTRS (r:rs) then return (r:rs) else Nothing 
    