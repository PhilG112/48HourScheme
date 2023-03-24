module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
        '\\' -> x
        '"' -> x
        'n' -> '\n'
        't' -> '\t'
        'r' -> '\r'

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    char '"'
    return $ String x

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number (fst $ head (readHex x))

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= \v -> return $ Number (read v)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
    try $ string "#d"
    x <- many1 digit
    (return . Number . read) x

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 octDigit
    return $ Number (fst $ head (readOct x))

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    x <- many1 (oneOf "01")
    return $ Number (bin2dig x)

bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) =
    let
        old = 2 * digint + (if x == '0' then 0 else 1)
    in
        bin2dig' old xs

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseDecimal2 <|> parseOct <|> parseBin <|> parseHex

-- Don't forget <$> is infix for fmap, can use that instead of liftM
parseNumberOld :: Parser LispVal
parseNumberOld = Number . read <$> many1 digit
-- parseNumber = liftM (Number . read) $ many1 digit

parseNumberDo :: Parser LispVal
parseNumberDo = do
    s <- many1 digit
    return $ Number $ read s

parseNumberBind :: Parser LispVal
parseNumberBind = many1 digit >>= return . Number . read

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space")
        <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        _ -> head value

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseBool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char
    deriving Show



