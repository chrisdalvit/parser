import qualified Data.Bifunctor
import System.Directory.Internal.Prelude (Show)
import Data.Char (isSpace, isDigit, ord)
import Text.Parsec.Prim (token)

class Monad m => MonadZero m where
    zero :: m a

class Monad m => MonadPlus m where
    (++) :: m a -> m a -> m a

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser a) = a

instance Functor Parser where
    fmap f p = Parser(map (Data.Bifunctor.first f) . parse p)

instance Applicative Parser where
    pure a = Parser(\cs -> [(a, cs)])
    p1 <*> p2 = Parser(concatMap (\(g,out) -> parse (fmap g p2) out) . parse p1)

instance Monad Parser where
    return a = Parser (\cs -> [(a, cs)])
    p >>= f = Parser (\cs -> concat [ parse (f a) cs' | (a, cs') <- parse p cs])

instance MonadZero Parser where
    zero = Parser (const [])

instance MonadPlus Parser where
    p1 ++ p2 = Parser (\cs -> parse p1 cs Prelude.++ parse p2 cs)

safeHead :: [a] -> [a]
safeHead [] = []
safeHead (x:xs) = [x]

(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser(safeHead . parse (p1 Main.++ p2))

-- split string in singelton list with tuple with head as fst and tail as snd
splitString :: String -> [(Char , String)]
splitString [] = []
splitString (x:xs) = [(x,xs)]

item :: Parser Char
item = Parser splitString

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else zero

-- parse specific character
char :: Char -> Parser Char
char c = sat (c==)

string :: String -> Parser String
string [] = return []
string (c:cs) = do
    char c
    string cs
    return (c:cs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
    a <- p
    as <- many p
    return (a:as)

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = sepby1 p sep +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep = do
    a <- p
    as <- many (do
        sep
        p
        )
    return (a:as)

chainl ::Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
    a <- p
    rest a
    where
        rest a = (do
            f <- op
            b <- p
            rest (f a b)) +++ return a

space :: Parser String
space = many $ sat isSpace

token :: Parser a -> Parser a
token p = do
    a <- p
    space
    return a

symb :: String -> Parser String
symb = Main.token . string

apply :: Parser a -> String -> [(a,String)]
apply p = parse (do
    space
    p
    )

expr  :: Parser Int
expr   = term   `chainl1` addop

addop :: Parser (Int -> Int -> Int)
addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}

mulop :: Parser (Int -> Int -> Int)
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return div}

term :: Parser Int
term   = factor `chainl1` mulop

factor :: Parser Int
factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}

digit :: Parser Int
digit  = do {x <- Main.token (sat isDigit); return (ord x - ord '0')}