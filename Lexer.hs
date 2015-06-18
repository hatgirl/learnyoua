
module Lexer where

import Data.Char
import Data.List


data Operator = Plus | Minus | Mult | Div | Mod | Exp  
              deriving ( Read, Show )
instance Eq Operator where
  Plus == Plus = True
  Minus == Minus = True
  Plus == Minus = True
  Minus == Plus = True
  Mult == Div = True
  Div == Mult = True
  Mult == Mult = True
  Div == Div = True
  Mod == Mod = True
  Exp == Exp = True
  _ == _ = False

instance Ord Operator where
  Plus < Minus = False
  Plus < Mult = True
  Plus < Div = True
  Plus < Exp = True
  Plus < Mod = True
  Minus < Mult = True
  Minus < Div = True
  Minus < Exp = True
  Minus < Mod = True
  Mult < Div = False
  Mult < Exp = True
  Mult < Mod = False
  Div < Exp = True
  Div < Mod = False
  Mod < Exp = True
  x < y = not (y < x) && (y /= x)
  x > y = not (x < y) && (y /= x)
  x >= y = (x > y) || (x == y)
  x <= y = (x < y) || (x == y)
  
data Separator = OParens | CParens | OBrack | CBrack deriving (Eq, Read, Show)
data Token = Op Operator | Lit Double | Sep Separator deriving (Eq, Read, Show)

lexOp :: String -> Token
lexOp "+" = Op Plus
lexOp "-" = Op Minus
lexOp "/" = Op Div
lexOp "%" = Op Mod
lexOp "*" = Op Mult
lexOp "^" = Op Exp
lexOp _ = error "Not a valid operation! "

lexSep :: String -> Token
lexSep "(" = Sep OParens
lexSep ")" = Sep CParens
lexSep "[" = Sep OBrack
lexSep "]" = Sep CBrack
lexSep _ = error "Not a valid operation! "

lexNum :: String -> Token
lexNum x = Lit $ read x

isOp :: Maybe Token -> Bool
isOp (Just (Op _ )) = True
isOp _ = False

isValidInt :: String -> Bool
isValidInt s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _ -> False


isValidDoub :: String -> Bool
isValidDoub s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _ -> False 


isValidOp :: String -> Bool
isValidOp x
  | x `elem` validOps = True
  | otherwise = False
  where validOps = ["+", "-", "*", "^", "/", "%", "_", "`"]

isValidOpChar :: Char -> Bool
isValidOpChar x
  | x `elem` validOps = True
  | otherwise = False
                where validOps  = ['+', '-', '*', '^', '/', '%', '_', '`']
                      
isValidSep :: String -> Bool
isValidSep x
  | x `elem` validSeps = True
  | otherwise = False
                where validSeps = ["(", ")", "[", "]"]

isValidSepChar :: Char -> Bool
isValidSepChar x
  | x `elem` validSeps = True
  | otherwise = False
              where validSeps = ['(', ')', '[', ']']
                    
-- We will not force operations to be separated by whitespace
-- so will need to separate out operations from strings of digits
splitOnOp :: String -> [String]
splitOnOp [] = [] 
splitOnOp [x] = [[x]]
splitOnOp (x:xs)
  | isValidOpChar x = [x]:splitOnOp xs
  | isValidSepChar x = [x]:splitOnOp xs
  | otherwise = if ((isValidOpChar $ head xs) || (isValidSepChar $ head xs))
                   then [x]:splitOnOp xs
                   else (x:(head $ splitOnOp xs)):(tail $ splitOnOp xs)


separateStrings :: [String] -> [String]
separateStrings [] = []
separateStrings [x] = splitOnOp x
separateStrings (x:xs) = (splitOnOp x)++(separateStrings xs)


tokenize ::String -> Token 
tokenize x
  | isValidOp x   = lexOp x
  | isValidSep x  = lexSep x
  | isValidDoub x = lexNum x
  | isValidInt x  = lexNum x
  | otherwise     = error "Not a valid symbol!"
  
tokenizeLine :: [String] -> [Token]
tokenizeLine []     = []
tokenizeLine [x]    = [tokenize x]
tokenizeLine (x:xs) = (tokenize x) : tokenizeLine xs

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial x = x*factorial (x-1)



  




