import Data.Char
import Data.List

main = do
  putStrLn "I'd like something to calculate please! "
  exprs <- getLine
  let tokens = tokenize exprs
  print tokens

--type Opmap = Map String Operator
data Operator = Plus | Minus | Mult | Div | Mod | Exp | Floor deriving (Show)
data Grouping = OParens | CParens | OBrack | CBrack
data Token a = Op Operator | Lit a | Grp Grouping deriving (Show)
data ParseTree a = Leaf a | Node (ParseTree a) (ParseTree a)

parseOp :: String -> Token a
parseOp "+" = Op Plus
parseOp "-" = Op Minus
parseOp "/" = Op Div
parseOp "%" = Op Mod
parseOp "*" = Op Mod
parseOp "^" = Op Exp
parseOp "_" = Op Floor
parseOp "`" = Op Ceil
parseOp _ = error "Not a valid operation! "

parseGrp :: String -> Token a
parseGrp "(" = Grouping OParens
parseGrp ")" = Grouping CParens
parseGrp "[" = Grouping OBrack
parseGrp "]" = Grouping CBrack

parseNum :: Read a => String -> Token a
parseNum x = Lit $ read x

isValidOp :: Char -> Bool
isValidOp x
  | x `elem` validOps = True
  | otherwise = False
  where validOps = ['+', '-', '*', '^', '/', '%', '_', '`']


parse :: String -> [Token a]
parse x = map parseOp $ map  splitOnOp $ words x
                 
-- We will not force operations to be separated by whitespace
-- so will need to separate out operations from strings of digits
splitOnOp :: String -> [String]
splitOnOp [] = []
splitOnOp [x] = [[x]]
splitOnOp (x:xs)
  | isValidOp x = [[]]++[[x]]++splitOnOp xs
  | otherwise = (x:(head $ splitOnOp xs)):(tail $ splitOnOp xs)

tokenize :: String -> [String]
tokenize []     = []
tokenize (x:xs) = undefined






  




