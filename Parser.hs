import Lexer
import Stack
import Queue
import Data.Fixed(mod')

main = do
  putStrLn "I'd like something to calculate please! "
  exprs <- getLine
  let sep = words exprs
  print sep
  let separated = separateStrings sep
  print separated 
  let tokens = tokenizeLine $ separateStrings $ words exprs
  print tokens
  let parsed = parse tokens (Queue [] []) (Stack [])
  print parsed
  let munched = munch parsed
  print munched


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
  | emptyS ops = parse xs output $ pushS ops $Op x 
  | otherwise  = parse xs (fst $ pushByPrecedence (Op x) (fst $ popS ops) output ops) (snd $ pushByPrecedence (Op x) ( fst $ popS ops ) output ops )




-- Helper function for parse
pushByPrecedence :: Token -> Token -> Queue Token -> Stack Token -> (Queue Token, Stack Token)
pushByPrecedence (Op x) (Op y) output ops
  | emptyS $snd $popS ops = if x <= y
                               then (pushQ output (Op y), pushS (snd $popS ops) $ Op x)
                               else (output, pushS ops $Op x)
  | x <= y    = pushByPrecedence (Op x) (fst $popS $snd $popS ops) (pushQ output $ Op y) (snd $ popS ops)
  | otherwise = ( output, pushS ops $Op x ) 
pushByPrecedence (Op x) _ output ops = (output, pushS ops $ Op x)
pushByPrecedence _ _ output ops = error "No defined behavior for token"




munch :: [Token] -> Double
munch [] = error "Nothing left to munch"
munch (Lit x:Lit y:Op z:[]) = eval (Lit x) (Lit y) (Op z)
munch (Lit x:Lit y:Op z:xs) = munch ((Lit $ eval (Lit x) (Lit y) (Op z)):xs)
munch (Lit x:xs) = eval (Lit x) (Lit $ munch $init xs) (last xs)


eval :: Token -> Token -> Token  -> Double
eval (Lit x) (Lit y) (Op Plus) = x + y
eval (Lit x) (Lit y) (Op Minus) = x - y
eval (Lit x) (Lit y) (Op Mult) = x*y
eval (Lit x) (Lit y) (Op Div) = x/y
eval (Lit x) (Lit y) (Op Mod) = x `mod'` y
eval (Lit x) (Lit y) (Op Exp) = x ** y
eval (Lit x) (Lit y) _ = error "Unknown operation"
eval (Lit x) _ _ = error "Need two numbers for binary operator"
eval _ (Lit y) _ = error "Need two numbers for binary operator"

 



