import Lexer
import Stack
import Queue
import Data.Fixed(mod')
 
parse :: [Token] -> (Queue Token) -> (Stack Token) -> [Token]
parse [] output ops
  | emptyS ops = qToList output
  | otherwise = parse [] (pushQ output $ fst $popS ops) (snd $popS ops)

parse (Sep OParens:xs) output ops = parse xs output $ pushS ops $Sep OParens
parse tokens@(Sep CParens:xs) output ops
  | emptyS ops = error "Mismatched parenthesis"
  | peekS ops == Just (Sep OParens) = parse xs output $ snd $ popS ops
  | otherwise = parse tokens ( pushQ output $ fst $popS ops) (snd $popS ops)

parse (Sep OBrack:xs) output ops = parse xs output $pushS ops $Sep OBrack
parse tokens@(Sep CBrack:xs) output ops
  | emptyS ops = error "Mismatched brackets"
  | peekS ops == Just (Sep OBrack) = parse xs output $ snd $ popS ops
  | otherwise = parse tokens ( pushQ output $ fst $popS ops) (snd $popS ops)

parse (Lit x:xs) output ops = parse xs (pushQ output $ Lit x) ops

parse (Op Exp:xs) output ops = parse xs output $ pushS ops $ Op Exp
    
parse tokens@(Op x:xs) output ops
  | isOp $ peekS ops =
    if (Op x::Token <= (fst (popS ops)))
       then  parse tokens (pushQ output $ fst ( popS ops)) (snd (popS ops))
       else  parse xs output $ pushS ops (Op x) 
  | otherwise = parse xs output (pushS ops (Op x))




munch :: [Token] -> Double
munch [] = error "Nothing left to munch"
munch (x:y:z:[]) = eval x y z
munch (x:y:z:xs) = munch $ (Lit $eval x y z):xs

eval :: Token -> Token -> Token  -> Double
eval (Lit x) (Lit y) (Op Plus) = x + y
eval (Lit x) (Lit y) (Op Minus) = x - y
eval (Lit x) (Lit y) (Op Mult) = x * y
eval (Lit x) (Lit y) (Op Div) = x / y
eval (Lit x) (Lit y) (Op Mod) = x `mod'` y
eval (Lit x) (Lit y) (Op Exp) = x ** y
eval (Lit x) (Lit y) _ = error "Unknown operation"
eval (Lit x) _ _ = error "Need two numbers for binary operator"
eval _ (Lit y) _ = error "Need two numbers for binary operator"

 



