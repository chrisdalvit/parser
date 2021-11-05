module Utils.Precedence where

import Utils.Parser
import GHC.Unicode (isAlphaNum)

newtype Precedence = Pred [(String, String)] deriving Eq

instance Show Precedence where
    show (Pred []) = ""
    show (Pred [(x,y)]) = x ++ " > " ++ y
    show (Pred ((x,y):xs)) = x ++ " > " ++ y ++ ", " ++ show (Pred xs)

parsePrecedencePart :: Parser (String, String)
parsePrecedencePart = do
    f <- token $ many (sat isAlphaNum)
    char '>'
    spaces
    g <- many (sat isAlphaNum)
    return (f, g)

parsePrecedence :: Parser Precedence
parsePrecedence = do
    p <- sep1 parsePrecedencePart space
    return $ Pred p

stringToPrecedence :: String -> Maybe Precedence
stringToPrecedence s = case parse parsePrecedence s of
  Nothing -> Nothing
  Just (p, []) -> Just p
  Just (p, x:xs) -> Nothing

argsToPrecedence :: [String] -> Maybe Precedence
argsToPrecedence [] = return $ Pred []
argsToPrecedence (x:xs) = do
    Pred a <- stringToPrecedence x
    Pred as <- argsToPrecedence xs
    return $ Pred (a ++ as)
