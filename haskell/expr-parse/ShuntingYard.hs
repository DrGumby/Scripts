module ShuntingYard where

import Data.Char
import Data.Map

data Token = Number Double
           | LParen
           | RParen
           | Add
           | Sub
           | Mult
           | Div
           deriving(Show, Eq)

-- Read a double from string and return rest of string
parseNumber :: String -> (Double, String)
parseNumber xs = head (reads xs :: [(Double, String)])

-- Hash map of all supported operators
operatorMap :: Map Char Token
operatorMap = fromList
              [
                ('+', Add),
                ('-', Sub),
                ('*', Mult),
                ('/', Div),
                ('(', LParen),
                (')', RParen)
              ]

prec :: Token -> Int
prec Add  = 1
prec Sub  = 1
prec Mult = 2
prec Div  = 2
prec LParen = 3
prec RParen = 3
prec _ = 4

isOp :: Token -> Bool
isOp Add = True
isOp Sub = True
isOp Mult = True
isOp Div = True
isOp _ = False

getFn :: Token -> (Double -> Double -> Double)
getFn Add = (+)
getFn Sub = (-)
getFn Mult = (*)
getFn Div = (/)
getFn _ = error "Invalid operation"

lowerPrec :: Token -> Token -> Bool
lowerPrec x y = isOp x && isOp y && (prec x) < (prec y)

-- Parse input string into list of tokens.
-- If a token is invalid, it is converted to Nothing
tokenize :: String -> [Maybe Token]
tokenize [] = []
tokenize (x:xs)
  | isDigit x = let (n, rest) = parseNumber (x:xs)
                in tokenize rest ++ [Just $ Number n]
  | otherwise = tokenize xs ++ [Data.Map.lookup x operatorMap]

-- Perform one shunt of the shunting yard algorithm
shunt :: [Maybe Token] -> [Token] -> [Token] -> Maybe [Token]
shunt [] [] queue = Just queue
shunt [] stack queue = if head stack == LParen
                       then Nothing
                       else shunt [] (tail stack) (queue ++ [head stack])
shunt (x:xs) stack queue = case x of
  Nothing -> Nothing
  (Just (Number n)) -> shunt xs stack (queue ++ [Number n])
  (Just LParen) -> shunt xs (LParen:stack) queue
  (Just RParen) -> shunt xs stack' queue' where
    stack' = tail $ dropWhile (/= LParen) stack
    queue' = queue ++ takeWhile (/= LParen) stack
  (Just op) -> shunt xs stack' queue' where
    stack' = op : dropWhile (lowerPrec op) stack
    queue' = queue ++ takeWhile (lowerPrec op) stack

transform :: [Maybe Token] -> Maybe [Token]
transform ts = shunt ts [] []


evaluate' :: Maybe [Token] -> [Double] -> Maybe Double
evaluate' Nothing _ = Nothing
evaluate' (Just []) stack = Just $ head stack
evaluate' (Just (x:xs)) stack = case x of
  (Number n) -> evaluate' (Just xs) (n:stack)
  op -> evaluate' (Just xs) stack' where
   op1 = stack !! 0
   op2 = stack !! 1
   result = (getFn op) op1 op2
   stack' = result : drop 2 stack

evaluate :: Maybe [Token] -> Maybe Double
evaluate Nothing = Nothing
evaluate rpn = evaluate' rpn []
