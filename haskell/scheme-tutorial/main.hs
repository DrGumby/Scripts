-- main.hs
-- Haskell tutorial from here:
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"" <|> (char '\\' >> char '\"'))
  char '"'
  return $ String x



parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number <$> (read <$> many1 digit)
-- parseNumber = read <$> many1 digit >>= return . Number
-- parseNumber = do
--   val <- read <$> many1 digit
--   return $ Number val

-- parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseNumber
         <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No Match: " ++ show err
  Right val -> "Found value" ++ show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
 
sumNumbers :: String -> String -> Int
sumNumbers a b = read a + read b
