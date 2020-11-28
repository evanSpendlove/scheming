module Main where
import Control.Monad
import Numeric
import System.Environment
import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+ -/: <=? >@^_~#"

-- read is for converting a string to a number
-- show is for converting a symbol to a string

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] | LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Char Char
             deriving Show

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedChars <|> (noneOf "\\\""))
                 char '"'
                 return $ String x

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"ntrv"
                  return $ case x of
                                '\\' -> x
                                '"'  -> x
                                'n'  -> '\n'
                                't'  -> '\t'
                                'r'  -> '\r'
                                'v'  -> '\v'

parseChar :: Parser LispVal
parseChar = liftM Char (parseSpecialCharNotation <|> parseSingleChar)

parseSingleChar :: Parser Char
parseSingleChar = do string "#\\"
                     x <- letter
                     return x

parseSpecialCharNotation :: Parser Char
parseSpecialCharNotation = do string "#\\"
                              x <- (parseSpace <|> parseNewLine)
                              return x

parseSpace :: Parser Char
parseSpace = do char 's'
                char 'p'
                char 'a'
                char 'c'
                char 'e'
                return ' '

parseNewLine :: Parser Char
parseNewLine = do char 'n'
                  char 'e'
                  char 'w'
                  char 'l'
                  char 'i'
                  char 'n'
                  char 'e'
                  return '\n'

parseFloat :: Parser LispVal
parseFloat = do
                x <- many1 digit
                char '.'
                y <- many1 digit
                let atom = (x ++ "." ++ y)
                return $ Float $ read atom

parseInteger :: Parser LispVal
parseInteger = parseNumber <|> parseRadixNumber

parseNumber :: Parser LispVal
parseNumber = do digits <- many1 digit
                 return $ (Number . read) digits

parseRadixNumber :: Parser LispVal
parseRadixNumber = do base <- parseBaseInteger
                      (case base of
                                 'b' -> parseBinaryNumber
                                 'o' -> parseOctalNumber
                                 'x' -> parseHexNumber
                                 otherwise -> parseNumber)

parseBaseInteger :: Parser Char
parseBaseInteger = do char 'x'
                      x <- (oneOf "xdob") -- hex | decimal | octal | binary
                      return x

parseBinaryNumber :: Parser LispVal
parseBinaryNumber = do digits <- many (oneOf "01") 
                       return $ Number (fst (head (readInt 2 (`elem` "01") digitToInt digits))) 

parseHexNumber :: Parser LispVal
parseHexNumber = do many (oneOf "0123456789ABCDEF") >>= \x -> return $ Number (fst (head (readHex x)))

parseOctalNumber :: Parser LispVal
parseOctalNumber = do digits <- many (oneOf "01234567") 
                      return $ Number (fst (head (readOct digits)))

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
parseExpr = parseChar
        <|> parseString
        <|> parseFloat
        <|> parseInteger
        <|> parseAtom

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
