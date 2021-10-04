module TermParser (Term, parse, parseFunc, stringToTerm) where

import System.Directory.Internal.Prelude (Show, Applicative, isAlpha)
import Control.Monad.Writer.Strict (Functor)
import Distribution.PackageDescription.Check (PackageCheck)
import Control.Applicative (Alternative (empty))
import GHC.Base ( Alternative(empty, (<|>)) )
import Data.Char (isSpace, isAsciiLower)

data Term a = Func String [Term a] | Var a deriving Show
data Rule a = Rule (Term a) (Term a)

instance Functor Term where
  fmap f (Var x) = Var (f x)
  fmap f (Func x args) = Func x (map (fmap f) args)

newtype Parser a = Parser (String -> Maybe (a, String))

instance Functor Parser where
    fmap f (Parser a) = Parser (\cs ->  case a cs of
            Nothing -> Nothing
            Just (a, s) -> Just (f a, s)
        )

instance Applicative Parser where
  pure a = Parser (\cs -> Just (a, cs))
  p1 <*> p2 = Parser (\cs -> do
        (f, xs) <- parse p1 cs
        (a, xs') <- parse p2 xs
        return (f a, xs')
    )

instance Monad Parser where
  p >>= f = Parser (\cs -> case parse p cs of
        Nothing -> Nothing
        Just (a,xs) -> parse (f a) xs
      )

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser (\cs -> case parse p1 cs of
        Nothing -> parse p2 cs
        Just a -> return a
      )


-- |Apply a given parser to a string
parse :: Parser a -> String -> Maybe (a, String)
parse (Parser a) = a

splitString :: String -> Maybe (Char, String)
splitString [] = Nothing
splitString (x:xs) = Just (x,xs)

-- |Parse one character
item :: Parser Char
item = Parser splitString

-- |Parser for parsing one item if it satisfies predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

-- |Apply a given parser zero or multiple times
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

-- |Apply a given parser one or multiple times
many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- many p
    return (x:xs)

-- |Given two parsers, sep returns a parser that parses the first parser seperated by the second parser zero or more times
sep :: Parser a -> Parser b -> Parser [a]
sep p1 p2 = sep1 p1 p2 <|> return []

-- |Given two parsers, sep1 returns a parser that parses the first parser seperated by the second parser at least once
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p1 p2 = do
    x <- p1
    xs <- many (do
        p2
        p1
        )
    return (x:xs)

-- |Parser for parsing one given char
char :: Char -> Parser Char
char c = sat (==c)

-- |Parser for parsing a given string
string :: String -> Parser String
string [] = return []
string (x:xs) = do
    c <- char x
    cs <- string xs
    return (c:cs)

-- |Parser for parsing a single space
space :: Parser Char
space = sat isSpace

-- |Parser for parsing multiple consecutive spaces
spaces :: Parser String
spaces = many space

-- |Parser for parsing a single comma character
comma :: Parser Char
comma = sat (==',')

-- |Term parser, for either parsing a function or a variable
parseTerm :: Parser (Term String)
parseTerm = parseFunc <|> parseVar

-- |Parser for parsing a term variable, variables are assumed to be ASCII lower-case characters
parseVar :: Parser (Term String)
parseVar = do
    spaces
    v <- sat isAsciiLower
    spaces
    return (Var [v])

-- |Parser for parsing a term function
parseFunc :: Parser (Term String)
parseFunc = do
    spaces
    f <- item
    spaces
    char '('
    spaces
    ts <- sep parseTerm comma
    spaces
    char ')'
    spaces
    return (Func [f] ts)

parseRule :: Parser (Rule String)
parseRule = do
    spaces
    lhs <- parseFunc
    spaces
    string "->"
    spaces
    Rule lhs <$> parseTerm

-- |Function that trys to parse a given string into a term
stringToTerm :: String -> Maybe (Term String)
stringToTerm s = case parse parseTerm s of
    Nothing -> Nothing
    Just(x, c:cs) -> Nothing
    Just(x, []) -> return x