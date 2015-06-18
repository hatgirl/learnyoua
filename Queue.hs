module Queue where

data Queue a = Queue [a] [a] deriving (Eq, Show, Read)

emptyQ :: Queue a -> Bool
emptyQ (Queue [] []) = True
emptyQ _           = False

pushQ :: Queue a -> a -> Queue a
pushQ (Queue enq deq) x = (Queue (x:enq) deq)

popQ :: Queue a -> ( a, Queue a )
popQ (Queue [] []) = error "Queue is empty \n"
popQ (Queue enq []) = popQ (Queue [] $ reverse enq)
popQ (Queue enq (x:xs)) = ( x, (Queue enq xs) )

qToList :: Queue a -> [a]
qToList (Queue enq deq) = deq ++ reverse enq
