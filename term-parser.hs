import System.Directory.Internal.Prelude (Show, Applicative)
import Control.Monad.Writer.Strict (Functor)
import Distribution.PackageDescription.Check (PackageCheck)
import Control.Applicative (Alternative (empty))
import GHC.Base ( Alternative(empty, (<|>)) )


data Term = Func String [Term] | Var String deriving Show

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


-- apply a given parser to a string
parse :: Parser a -> String -> Maybe (a, String)
parse (Parser a) = a 

splitString :: String -> Maybe (Char, String)
splitString [] = Nothing
splitString (x:xs) = Just (x,xs)

-- parse one character
item :: Parser Char 
item = Parser splitString

sat :: (Char -> Bool) -> Parser Char 
sat p = do
    x <- item
    if p x then return x else empty 
