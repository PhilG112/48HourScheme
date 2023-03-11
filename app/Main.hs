module Main where

import Data.Functor
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled

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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number (fst $ head (readHex x))

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= (return . Number . read)

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
parseNumber = parseDecimal
            <|> parseDecimal2
            <|> parseOct
            <|> parseBin
            <|> parseHex

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
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    -- <|> parseBool
    <|> do
            char '('
            x <- try parseList <|> parseDottedList
            char ')'
            return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String c) = "\"" ++ c ++ "\""
showVal (Atom a) = a
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List c) = "(" ++ unwordsList c ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
                ("<", numBoolBinop (<)),
                (">", numBoolBinop (>)),
                ("/=", numBoolBinop (/=)),
                (">=", numBoolBinop (>=)),
                ("<=", numBoolBinop (<=)),
                ("&&", boolBoolBinop (&&)),
                ("||", boolBoolBinop (||)),
                ("string=?", strBoolBinop (==)),
                ("string<?", strBoolBinop (<)),
                ("string>?", strBoolBinop (>)),
                ("string<=?", strBoolBinop (<=)),
                ("string>=?", strBoolBinop (>=))]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do
                                    left <- unpacker $ head args
                                    right <- unpacker $ args !! 1
                                    return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop f args = boolBinop unpackNum f args
-- numBoolBinop = boolBinop unpackNum

strBoolBinop :: (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop f args = boolBinop unpackStr f args

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr errValue = throwError $ TypeMismatch "string" errValue

boolBoolBinop :: (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop f args = boolBinop unpackBool f args

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op [x] = throwError $ NumArgs 2 [x]
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
-- numericBinop op params = mapM unpackNum params <&> (Number . foldl1 op)
-- numericBinop op params =  Number . foldl1 op <$> mapM unpackNum params 
-- numericBinop op params = return $ Number $ foldl1 op $ map unpackNum params -- Old

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                            if null parsed
                                then throwError $ TypeMismatch "numebr" $ String n
                                else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val