module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+ -/: <=? >@^_~#"

-- read is for converting a string to a number
-- show is for converting a symbol to a string

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] | LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             deriving Show

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseFloat :: Parser LispVal
parseFloat = do
                x <- many1 digit
                char '.'
                y <- many1 digit
                let atom = (x ++ "." ++ y)
                return $ Float $ read atom

parseNumber :: Parser LispVal
parseNumber = do
                x <- many1 digit
                return $ Number $ read x 

-- <|> is the choice operator
-- Basically tries the first parser, then second if first fails, etc.
-- If either succeeds, returns the value from that parser.
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t"      -> Bool True
                          "#f"      -> Bool False
                          otherwise -> Atom atom

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseFloat
        <|> parseNumber
