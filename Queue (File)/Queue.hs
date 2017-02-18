--// =======================================================================//--
--// 						      Adil Khali      					        //--      
--// =======================================================================//--

module Queue (Queue, emptyQueue, queueEmpty, enqueue, dequeue, front) where

newtype Queue a = Qu [a]
	deriving (Show)

emptyQueue :: Queue a
emptyQueue = Qu []

queueEmpty :: Queue a -> Bool
queueEmpty  (Qu []) = True
queueEmpty  _ = False

enqueue :: a -> Queue a -> Queue a
enqueue x (Qu q) = Qu(q ++ [x])

dequeue :: Queue a -> Queue a
dequeue (Qu(x:q)) = Qu(q)

front :: Queue a -> a
front (Qu(x:q)) = x

