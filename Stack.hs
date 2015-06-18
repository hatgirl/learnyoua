module Stack where

data Stack a = Stack [a] deriving (Eq, Show, Read)

emptyS :: Stack a -> Bool
emptyS (Stack []) = True
emptyS _ = False

pushS :: Stack a -> a -> Stack a
pushS (Stack xs) x = Stack (x:xs)

popS :: Stack a -> (a, Stack a)
popS (Stack []) = error "Stack is empty \n"
popS (Stack (x:xs)) = (x, Stack xs)  

peekS :: Stack a -> Maybe a
peekS (Stack []) = Nothing
peekS (Stack (x:_)) = Just x
