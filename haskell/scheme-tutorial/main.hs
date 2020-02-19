-- main.hs
-- Haskell tutorial from here:
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Control.Monad.Except

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char

instance Show LispVal where show = showVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseSpecialChar :: Parser Char
parseSpecialChar = char '\\' >> oneOf "ntr\\\""

parseChar :: Parser LispVal
parseChar = do
  char '#'
  char '\\'
  c <- letter <|> symbol <|> digit
  return $ Character c

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"" <|> parseSpecialChar)
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

parseDecimal :: Parser LispVal
parseDecimal = Number <$> (read <$> many1 digit)

bintodec :: Integral i => i -> i
bintodec 0 = 0
bintodec i = 2 * bintodec (div i 10) + (mod i 10)

parseWithBase :: Parser LispVal
parseWithBase = do
  char '#'
  base <- oneOf "boxd"
  retval <- case base of
    'b' -> bintodec . read <$> many1 digit
    'o' -> fst . head . readOct <$> many1 digit
    'x' -> fst . head . readHex <$> many (oneOf "0123456789abcdefABCDEF") 
    'd' -> read <$> many1 digit
  return $ Number retval
  

parseNumber :: Parser LispVal
parseNumber = parseWithBase <|> parseDecimal
-- parseNumber = Number <$> (read <$> many1 digit)
-- parseNumber = read <$> many1 digit >>= return . Number
-- parseNumber = do
--   val <- read <$> many1 digit
--   return $ Number val

-- parseNumber = liftM (Number . read) $ many1 digit

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

parseExpr :: Parser LispVal
parseExpr =  parseNumber
         <|> parseChar
         <|> parseAtom
         <|> parseString
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> String $ "No Match: " ++ show err
  Right val -> val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives


primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("div", numericBinop div),
              ("rem", numericBinop rem),
              ("quot", numericBinop quot)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                         if null parsed
                            then 0
                         else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

sumNumbers :: String -> String -> Int
sumNumbers a b = read a + read b
