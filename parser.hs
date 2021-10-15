module Parser (Parser, parse, item, char, string, comma, sep, sat, token, space, many1, many, spaces, items, argWords, split, parseUntil) where

import Control.Applicative (Alternative)
import GHC.Base (Alternative(empty, (<|>)))
import Data.Char (isSpace, isAlphaNum)

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

-- |Parse characters to first non alpha-numeric character
items :: Parser String
items = do
    c <- sat isAlphaNum
    cs <- items
    return (c:cs)

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

-- |Parser that removes leading and trailing spaces
token :: Parser a -> Parser a
token p = do
    spaces
    x <- p
    spaces
    return x

parseUntil :: Char -> Parser String
parseUntil c = Parser(Just . span (/=c))

rest :: Parser String
rest = Parser saveHead

saveHead :: String -> Maybe (String, String)
saveHead [] = Just ([], [])
saveHead (x:xs) = Just ([x], xs)

split :: Char -> Parser (String, String)
split c = do
    h <- parseUntil c
    char c
    r <- rest 
    return (h,r)

argWords :: Parser [String]
argWords = Parser(\cs -> Just (words cs, ""))