import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Bool Bool

instance Show LispVal where
    show (Number x) = show x
    show (Atom x) = x
    show (String x) = "\"" ++ x ++ "\""
    show (Char x) = show x
    show (Bool True) = "true"
    show (Bool False) = "false"
    show (List x) = "(" ++ unwords (map show x) ++ ")"
    show (DottedList x y) = "<dotted>"

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseChar :: Parser LispVal
parseChar = do
              char '\\'
              char '#'
              x <- letter
              return $ Char x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
              x <- many1 digit
              return $ Number $ read x

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseChar

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
        (expr:_) <- getArgs
        putStrLn (readExpr expr)
